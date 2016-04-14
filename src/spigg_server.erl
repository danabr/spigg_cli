-module(spigg_server).

-record(state, {db :: spigg:db() }).

-behavior(gen_server).

%% API
-export([ dump/1
        , lookup/1
        , merge/1
        , start_link/0
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

%% API
dump(Path) when is_list(Path) ->
  gen_server:call({?MODULE, ?NODE}, {dump, Path}).

lookup({_, _, _}=MFA) ->
  gen_server:call({?MODULE, ?NODE}, {lookup, MFA}).

merge(DB) ->
  gen_server:call({?MODULE, ?NODE}, {merge, DB}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

stop() ->
  gen_server:call({?MODULE, ?NODE}, stop).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({dump, Path}, _From, #state{db=DB}=State)      ->
  Res = file:write_file(Path, io_lib:format("~p.", [DB])),
  {reply, Res, State};
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
