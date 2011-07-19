-module(erlstats_test).
-include_lib("eunit/include/eunit.hrl").

-define(STAT1, erlstats_test_1).
-define(STAT2, erlstats_test_2).

t_register_stat() ->
    ?assertEqual(true, erlstats:register_stat(?STAT1, value)),
    ?assertEqual(false, erlstats:register_stat(?STAT1, value)),
    ?assertEqual(true, erlstats:register_stat(?STAT2, counter)),
    ?assertEqual(false, erlstats:register_stat(?STAT2, counter)).

t_insert_stats() ->
    ?assertEqual(1, erlstats:increment_stat(?STAT2)),
    ?assertEqual(3, erlstats:increment_stat(?STAT2, 2)),
    ?assertEqual(test1, erlstats:update_stat(?STAT1, test1)),
    ?assertEqual(test2, erlstats:update_stat(?STAT1, test2)).

t_destroy_stat() ->
    ?assertEqual(true, erlstats:destroy_stat(?STAT1)),
    ?assertEqual(true, erlstats:destroy_stat(?STAT2)).

t_get_stat() ->
    ?assertEqual(true, erlstats:register_stat(?STAT1, value)),
    ?assertEqual(test1, erlstats:update_stat(?STAT1, test1)),
    ?assertEqual(test1, erlstats:get_stat(?STAT1)),
    ?assertEqual(true, erlstats:register_stat(?STAT2, counter)),
    ?assertEqual(15, erlstats:increment_stat(?STAT2, 15)),
    ?assertEqual(15, erlstats:get_stat(?STAT2)),
    ?assertEqual([{?STAT1, test1},
		  {?STAT2, 15}], erlstats:get_all_stats()),
    ?assertEqual(true, erlstats:destroy_stat(?STAT1)),
    ?assertEqual(true, erlstats:destroy_stat(?STAT2)).
    
t_register_stats() ->
    ?assertEqual([{?STAT1, true},
		  {?STAT2, true}], erlstats:register_stats([{?STAT1, value},
							    {?STAT2, counter}])).
t_destroy_stats() ->
    ?assertEqual([{?STAT1, true},
		  {?STAT2, true}], erlstats:destroy_stats([?STAT1, ?STAT2])).

t_reset_stat() ->
    ?assertEqual([{?STAT1, true},
		  {?STAT2, true}], erlstats:register_stats([{?STAT1, value},
							    {?STAT2, counter}])),
    ?assertEqual(test1, erlstats:update_stat(?STAT1, test1)),
    ?assertEqual(15, erlstats:increment_stat(?STAT2, 15)),
    ?assertEqual(true, erlstats:reset_stat(?STAT2)),
    ?assertEqual(0, erlstats:get_stat(?STAT2)),
    ?assertEqual(15, erlstats:increment_stat(?STAT2, 15)),
    ?assertEqual([{?STAT1, true},
		  {?STAT2, true}], erlstats:reset_stats([?STAT1, ?STAT2])),
    ?assertEqual([{?STAT1, undefined},
		  {?STAT2, 0}], erlstats:get_all_stats()).
    
    
erlstats_test_() ->
    {setup,
     fun() ->
	     application:start(erlstats)
     end,
     fun(_Pid) ->
	     application:stop(erlstats)
     end,
     [
      {"register a stat", ?_test(t_register_stat())},
      {"insert a few stats", ?_test(t_insert_stats())},
      {"destroy stats", ?_test(t_destroy_stat())},
      {"get stats", ?_test(t_get_stat())},
      {"register many stats", ?_test(t_register_stats())},
      {"delete many stats", ?_test(t_destroy_stats())},
      {"reset stat and stats", ?_test(t_reset_stat())}
     ]}.
