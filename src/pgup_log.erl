-module(pgup_log).

-export([msg/2, msg/1]).

msg(L) ->
	msg(L, []).

msg(List, Params) ->
	case io_lib:printable_unicode_list(List) of 
		true 	-> print([List], Params);
		_ 		-> print(List, Params)
	end.

print(List, Params) ->
	Output = string:join(List, "~n"),
	io:format(Output ++ "~n", Params).