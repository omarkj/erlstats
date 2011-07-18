-module(erlstats).
-behaviour(gen_server).

-export([start_link/0,
	 register_stat/2,
	 destroy_stat/1,
	 increment_stat/1,
	 increment_stat/2,
	 update_stat/2,
	 get_stat/1,
	 get_all_stats/0]).

%%% internal exports
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {}).
-define(SERVER, ?MODULE).
-define(STORAGE(), erlstats_utils:get_app_env(storage_backend, erlstats_ets)).

start_link() ->
    case erlstats_utils:get_app_env(register_global, false) of
	true ->
	    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []);
	_ ->
	    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])
    end.

-spec register_stat(StatName::atom(),
		    StatType::value | counter) ->
			   ok.
register_stat(StatName, Type) when Type =:= value;
				   Type =:= counter ->
    Storage = ?STORAGE(),
    case Storage:register_stat(StatName, Type) of
	{reply, true} ->
	    true;
	{reply, false} ->
	    false;
	{reply, badarg} ->
	    erlang:error(badarg, StatName)
    end.

-spec increment_stat(StatName::atom()) ->
			    {NewCount::integer(), StatName::atom()}.
increment_stat(StatName) ->
    increment_stat(StatName, 1).

-spec increment_stat(StatName::atom(),
		     Count::integer()) ->
			    {NewCount::integer(), StatName::atom()}.
increment_stat(StatName, Count) ->
    Storage = ?STORAGE(),
    case Storage:increment_stat(StatName, Count) of
	{reply, badarg} ->
	    erlang:error(badarg);
	{reply, NewCount} ->
	    NewCount
    end.

-spec update_stat(StatName::atom(),
		  NewValue::any()) ->
			 {NewValue::any(), StatName::any()}.
update_stat(StatName, NewValue) ->
    Storage = ?STORAGE(),
    case Storage:update_stat(StatName, NewValue) of
	{reply, badarg} ->
	    erlang:error(badarg);
	{reply, NewValue} ->
	    NewValue
    end.

-spec destroy_stat(StatName::atom()) ->
			  true.
destroy_stat(StatName) ->
    Storage = ?STORAGE(),
    case Storage:destroy_stat(StatName) of
	{reply, true} ->
	    true;
	{reply, badarg} ->
	    erlang:error(badarg)
    end.

%% @hidden
init([]) ->
    Storage = ?STORAGE(),
    ok = Storage:create_storage(),
    {ok, #state{}}.

%% @hidden
handle_call(get_all, _From, State) ->
    Stats = get_all_stats(),
    {reply, Stats, State};
handle_call({get, StatName}, _From, State) ->
    Stat = get_stat(StatName),
    {reply, Stat, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% Internal methods
get_stat(StatName) ->
    Storage = ?STORAGE(),
    case Storage:get_stat(StatName) of
	{reply, badarg, StatName} ->
	    erlang:error(badarg);
	{reply, Stat} ->
	    Stat
    end.

get_all_stats() ->
    Storage = ?STORAGE(),
    case Storage:get_all_stats() of
	{reply, badarg} ->
	    erlang:error(badarg);
	{reply, Stats} ->
	    Stats
    end.
