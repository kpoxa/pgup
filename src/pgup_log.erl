-module(pgup_log).

-export([msg/2, msg/1, ok/2, ok/1, err/2, err/1]).

ok(Str) -> 
	ok(Str, []).

ok(Str, Params) when is_list(Params) ->
	msg(cake:fg(lightgreen, io_lib:format(Str, Params))).

err(Str) -> 
	err(Str, []).

err(Str, Params) when is_list(Params) ->
	msg(cake:fg(lightred, io_lib:format(Str, Params))).

msg(Str) ->
	msg(Str, []).

msg(Str, Params) when is_list(Params) ->
	io:format(Str ++ "~n", Params).