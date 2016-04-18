-module(spigg_cli).

-export([main/2]).

main(Args, EbinPath) ->
  {ok, _} = net_kernel:start(['spigg_cli@127.0.0.1', longnames]),
  erlang:set_cookie(node(), 'spigg'),
  case net_adm:ping(spigg_node()) of
    pong -> ok;
    pang -> start_server(EbinPath)
  end,
  run_cmd(Args).
  
start_server(EbinPath) ->
  io:format("Booting spigg server.~n"),
  os:cmd("erl -detached -name " ++
         atom_to_list(spigg_node()) ++
         " -setcookie spigg -s spigg_server start_link -pa " ++ EbinPath),
  wait_until_up().

wait_until_up() ->
  Waiter = self(),
  WaitF = fun(Loop) ->
    case rpc:call(spigg_node(), erlang, registered, []) of
      L when is_list(L)  -> Waiter ! {self(), spigg_ready};
      {badrpc, nodedown} ->
        timer:sleep(200),
        Loop(Loop)
    end
  end,
  Pid = spawn(fun() -> WaitF(WaitF) end),
  receive
    {Pid, spigg_ready} ->
      io:format("Spigg server is up and running.~n")
  after
    3000 ->
      io:format("Spigg server failed to boot.~n"),
      halt(1)
  end.

run_cmd(["analyze"|[_|_]=Files]) -> analyze(Files);
run_cmd(["dump", Path]) ->
  case spigg_server:dump(Path) of
    ok   -> io:format("Database dumped to ~s.~n", [Path]);
    _Err -> io:format("Failed to write to ~s.~n", [Path])
  end;
run_cmd(["load", Path]) ->
  case spigg_server:load(Path) of
    ok   -> io:format("Loaded ~s.~n", [Path]);
    error -> io:format("Failed to load or parse ~s.~n", [Path])
  end;
run_cmd(["lookup", MFAStr, "--trace", SideEffectStr]) ->
  SideEffect = list_to_atom(SideEffectStr),
  trace(MFAStr, fun(X) -> X =:= SideEffect end);
run_cmd(["lookup", MFAStr, "--trace"]) ->
  trace(MFAStr, fun(_) -> true end);
run_cmd(["lookup", MFAStr]) ->
  case parse_mfa(MFAStr) of
    {ok, MFA} -> lookup(MFA);
    error     -> help()
  end;
run_cmd(["stop"])                ->
  try spigg_server:stop()
  catch
    exit:{normal, _} -> io:format("Stopped gracefully.~n");
    _:_              -> io:format("Failed to gracefully stop the server. "
                                  "Shutting down anyway.~n")
  end,
  ok = rpc:call(spigg_node(), init, stop, []);
run_cmd(_)                       -> help().

parse_mfa(MFAStr) ->
  case re:split(MFAStr, "[:/]", [{return, list}]) of
    [MStr, FStr, AStr] ->
      %% Note: We could avoid leaking atoms by sending raw strings to the server
      %% and having it run list_to_existing_atom. If that fails, it is safe to
      %% assume that the function has not been analyzed.
      M = list_to_atom(MStr),
      F = list_to_atom(FStr),
      case string:to_integer(AStr) of
        {A, []}    -> {ok, {M, F, A}};
        {error, _} -> error
      end;
    _                  -> error
  end.

analyze([])           -> ok;
analyze([File|Files]) ->
  Result = case spigg_analyze:beam(File) of
    {ok, DB}           ->
      ok = spigg_server:merge(DB),
      "ok";
    {error, not_found} -> "failed to read or parse"
  end,
  io:format("~s: ~s.~n", [File, Result]),
  analyze(Files).

lookup(MFA) ->
  {Micro, _} = timer:tc(fun() -> do_lookup(MFA) end),
  io:format("Operation completed in ~p s.~n", [Micro / 1000000]).

do_lookup({M, F, A}=MFA) ->
  case spigg_server:lookup(MFA) of
    {ok, {SideEffects, Unknowns}} ->
      print_side_effects(SideEffects),
      print_unknowns(Unknowns);
    {error, not_found} ->
      io:format("Function ~s:~s/~p has not been analyzed.~n",
                [M, F, A]),
      halt(1)
  end.

trace(MFAStr, TraceFun) ->
  case parse_mfa(MFAStr) of
    {ok, MFA} ->
      {Micro, _} = timer:tc(fun() -> do_trace(MFA, TraceFun) end),
      io:format("Operation completed in ~p s.~n", [Micro / 1000000]);
    error     -> help()
  end.

do_trace({M, F, A}=MFA, TraceFun) ->
  case spigg_server:trace(MFA, TraceFun) of
    {ok, {SideEffects, Unknowns}} ->
      print_side_effect_traces(SideEffects),
      print_unknowns(Unknowns);
    {error, not_found} ->
      io:format("Function ~s:~s/~p has not been analyzed.~n",
                [M, F, A]),
      halt(1)
  end.

print_side_effects([])          ->
  io:format("No side effects detected.~n");
print_side_effects(SideEffects) ->
  io:format("Side effects:~n"),
  lists:foreach(fun({Line, [], Effect}) ->
                      io:format("~p: ~p~n", [Line, Effect]);
                    ({Line, {M, F, A}, Effect}) ->
                      io:format("~p: ~p (~p:~p/~p)~n", [Line, Effect, M, F, A])
                end, SideEffects).

print_unknowns([])       -> ok;
print_unknowns(Unknowns) ->
  io:format("Unknown functions:~n"),
  lists:foreach(fun({M, F, A}) ->
    io:format("~p:~p/~p~n", [M, F, A])
  end, Unknowns).

print_side_effect_traces([])          ->
  io:format("No side effects matching the applied filters detected.~n");
print_side_effect_traces(SideEffects) ->
  io:format("Side effects:~n"),
  lists:foreach(fun({Line, [], Effect}) ->
                      io:format("~p: ~p~n", [Line, Effect]);
                    ({Line, MFAs, Effect}) ->
                      io:format("~p: ~p~n", [Line, Effect]),
                      print_trace(MFAs, "  ")
                end, SideEffects).

print_trace([], _Indentation)              -> ok;
print_trace([{M, F, A}|MFAs], Indentation) ->
  io:format("~s~p:~p/~p~n", [Indentation, M, F, A]),
  print_trace(MFAs, "  " ++ Indentation).

help() ->
  io:format("spigg <command> [args]~n~n", []),
  io:format("COMMANDS:~n"),
  io:format("analyze <file1> [<file2> ...]~n"),
  io:format("  Analyze the given beam files and add them to the database.~n"),
  io:format("dump <file>~n"),
  io:format("  Dump the database to the given file.~n"),
  io:format("help~n"),
  io:format("  Print this help text.~n"),
  io:format("load <file>~n"),
  io:format("  Load the database in the given file. The loaded database will~n"
            "  be merged with the existing database.~n"),
  io:format("lookup <Mod>:<Fun>/<Arity> [--trace [<side_effect>]]~n"),
  io:format("  Lookup the side effects of Mod:Fun/Arity.~n"),
  io:format("  If --trace is specified, spigg will the call chain leading to ~n"
            "  the side effect. If <side_effect> is specified, only traces ~n"
            "  matching that side effect will be printed.~n"),
  io:format("stop~n"),
  io:format("  Stop the spigg daemon.~n").

spigg_node() -> 'spigg@127.0.0.1'.
