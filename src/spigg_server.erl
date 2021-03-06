-module(spigg_server).

-record(state, {db :: spigg:db() }).

-behavior(gen_server).

%% API
-export([ dump/1
        , load/1
        , lookup/1
        , merge/1
        , start_link/0
        , trace/2
        , stop/0
        ]).

%% gen_server callbacks
-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-define(NODE, 'spigg@127.0.0.1').
-define(CALL_TIMEOUT, 30000).

%% API
dump(Path) when is_list(Path) ->
  call({dump, Path}).

load(Path) when is_list(Path) ->
  call({load, Path}).

lookup({_, _, _}=MFA) ->
  case call({lookup, MFA}) of
    {ok, {SideEffectsWithTraces, Unknown}} ->
      SideEffects = top_level_side_effects(SideEffectsWithTraces, []),
      {ok, {SideEffects, Unknown}};
    {error, not_found} = Err               -> Err
  end.

merge(DB) ->
  call({merge, DB}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

trace({_, _, _}=MFA, TraceFun) ->
  case call({lookup, MFA}) of
    {ok, {SideEffects, Unknowns}} ->
      Filtered = filter_side_effects(SideEffects, TraceFun),
      {ok, {Filtered, Unknowns}};
    {error, not_found} = Err      -> Err
  end.

stop() ->
  call(stop).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({dump, Path}, _From, #state{db=DB}=State)      ->
  Res = file:write_file(Path, io_lib:format("~p.", [DB])),
  {reply, Res, State};
handle_call({load, Path}, _From, #state{db=OldDB}=State)   ->
  case file:consult(Path) of
    {ok, [NewDB]} ->
      try spigg:merge(OldDB, NewDB) of
        CombinedDB -> {reply, ok, State#state{db=CombinedDB}}
      catch
        _:_ ->
        {reply, error, State}
      end;
    _Err          ->
      {reply, error, State}
  end;
handle_call({lookup, MFA}, _From, #state{db=DB}=State)     ->
  {reply, spigg:side_effects(DB, MFA), State};
handle_call({merge, NewDB}, _From, #state{db=OldDB}=State) ->
  {reply, ok, State#state{db=spigg:merge(OldDB, NewDB)}};
handle_call(stop, _From, State)                            ->
  {stop, normal, State}.

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

handle_info(Msg, State) ->
  {stop, {unexpected_info, Msg}, State}.

init(_Args) ->
  {ok, #state{db = spigg:new_db()}}.

terminate(_Reason, _State) -> ok.

%% Internal
call(Msg) ->
  gen_server:call({?MODULE, ?NODE}, Msg, ?CALL_TIMEOUT).

top_level_side_effects([], TopLevel)                                        ->
  TopLevel;
top_level_side_effects([{Line, [], Effect}|SideEffects], TopLevel)          ->
  SideEffect = {Line, local, Effect},
  top_level_side_effects(SideEffects,
                         ordsets:add_element(SideEffect, TopLevel));
top_level_side_effects([{Line, [MFA|_MFAs], Effect}|SideEffects], TopLevel) ->
  SideEffect = {Line, MFA, Effect},
  top_level_side_effects(SideEffects,
                         ordsets:add_element(SideEffect, TopLevel)).

filter_side_effects(SideEffects, Filter) ->
  [ SE || {_Line, _Trace, Effect}=SE <- SideEffects, Filter(Effect) ].
