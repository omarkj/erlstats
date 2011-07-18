-module(erlstats_utils).

-export([get_app_env/2]).

get_app_env(Key, Default) ->
    case application:get_env(Key) of
	undefined ->
	    Default;
	{ok, undefined} ->
	    Default;
	{ok, Value} ->
	    Value
    end.
