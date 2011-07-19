-module(erlstats_ets).

-behaviour(gen_erlstats).

-define(TAB, erlstats).

-export([create_storage/0,
	 register_stat/2,
	 increment_stat/2,
	 update_stat/2,
	 destroy_stat/1,
	 get_stat/1,
	 get_all_stats/0,
	 reset_stat/1]).

-spec create_storage() ->
			    ok.
create_storage() ->
    Options = erlstats_utils:get_app_env(ets_options, [{write_concurrency,true},
						       {read_concurrency, true}]),
    create_ets(Options).

-spec register_stat(StatName::atom(),
		    StatType::counter | value) ->
			   {reply, true} |
			   {reply, badarg}.
register_stat(StatName, counter) ->
    create_key({StatName, counter, 0});
register_stat(StatName, value) ->
    create_key({StatName, value, undefined}).

-spec increment_stat(StatName::atom(),
		     IncrementBy::integer()) ->
			    {reply, NewCount::integer(), StatName::atom()}.
increment_stat(StatName, IncrementBy) ->
    try
	NewCount = ets:update_counter(?TAB, StatName, {3, IncrementBy}),
	{reply, NewCount}
     catch
	 error:_ ->
	     {reply, badarg}
     end.

-spec update_stat(StatName::atom(),
		  Value::any()) ->
			 {reply, Value::any(), StatName::atom()} |
			 {reply, badarg, StatName::atom()}.
update_stat(StatName, Value) ->
    try
	case ets:update_element(?TAB, StatName, {3, Value}) of
	    true ->
		{reply, Value};
	    false ->
		{reply, badarg}
	end
    catch
	error:_ ->
	    {reply, badarg}
    end.

-spec destroy_stat(StatName::atom()) ->
			  {reply, true} |
			  {reply, badarg}.
destroy_stat(StatName) ->
    try
	ets:delete(?TAB, StatName),
	{reply, true}
    catch
	error:_ ->
	    {reply, badarg}
    end.

-spec get_stat(StatName::atom()) ->
		      {reply, Stat::any()} |
		      {reply, badarg, StatName::atom()}.
get_stat(StatName) ->
    try
	case ets:lookup(?TAB, StatName) of
	    [{StatName, _Type, Value}] ->
		{reply, Value};
	    [] ->
		{reply, badarg}
	end
    catch
	error:_ ->
	    {reply, badarg}
    end.

-spec get_all_stats() ->
			   {reply, [any()]} |
			   {reply, badarg}.
get_all_stats() ->
    try
	Stats = lists:map(fun({StatName, _Type, Value}) ->
				  {StatName, Value}
			  end, ets:tab2list(?TAB)),
	{reply, Stats}
    catch
	error:_ ->
	    {reply, badarg}
    end.

-spec reset_stat(StatName::atom()) ->
			{reply, undefined} |
			{reply, 0}.
reset_stat(StatName) ->
    try
	case ets:lookup(?TAB, StatName) of
	    [{StatName, value, _Value}] ->
		clear_value(StatName);
	    [{StatName, counter, _Value}] ->
		clear_counter(StatName)
	end
    catch
	error:_ ->
	    {reply, badarg}
    end.

%% Internal
create_ets(Options) ->
    case ets:info(?TAB) of
	undefined ->
	    ets:new(?TAB, [ordered_set, public, named_table | Options]),
	    ok;
	_ ->
	    ok
    end.

create_key(Object) ->
        try
	    case ets:insert_new(?TAB, Object) of
		false ->
		    {reply, false};
		true ->
		    {reply, true}
	    end
	catch
	    error:_ ->
		{reply, badarg}
	end.

clear_value(StatName) ->
    case update_stat(StatName, undefined) of
	{reply, undefined} ->
	    {reply, true};
	_ ->
	    {reply, badarg}
    end.

clear_counter(StatName) ->
    case update_stat(StatName, 0) of
	{reply, 0} ->
	    {reply, true};
	_ ->
	    {reply, badarg}
    end.
