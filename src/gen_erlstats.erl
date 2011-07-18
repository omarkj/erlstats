-module(gen_erlstats).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{create_storage, 0},
     {register_stat, 2},
     {increment_stat, 2},
     {update_stat, 2},
     {destroy_stat, 1},
     {get_stat, 1},
     {get_all_stats, 0}];
behaviour_info(_) ->
    undefined.

