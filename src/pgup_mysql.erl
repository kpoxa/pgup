-module(pgup_mysql).
-behaviour (pgup_db_behaviour).
-export([query/3, connect/1, init_db_schema/1, current_version/1, record_version/2, remove_version/2, master_db/0, create_db/2]).

master_db() ->
	"mysql".

connect(Cfg) -> 
	Host = proplists:get_value(host, Cfg, "127.0.0.1"),
	User = proplists:get_value(user, Cfg, "sa"),
	Db	 = proplists:get_value(db, Cfg),
	Pass = proplists:get_value(pass, Cfg, ""),
	Opts = proplists:get_value(opts, Cfg, []),
	pgup_log:msg(["Connecting to ~p with user ~p and db ~p..."], [Host, User, Db]),
	Pid = connection_result(connect(Host, User, Db, Pass, Opts)),
	{Cfg, Pid}.

create_db( {_Cnf, Con}, Db ) ->
	pgup_log:msg("Creating database ~p", [Db]),
	result(mysql:query(Con, "create database " ++ Db)).

connection_result({ok, Pid}) when is_pid(Pid) ->
	Pid;
connection_result({error, {Code, _, Text}}) ->
	pgup_log:msg("~s ~p", [cake:fg(lightred, reason_to_str(Text)), Code]),
	throw(sql_error).
	
connect(_, _, undefined, _, _ ) ->
	pgup_log:msg(["	Database name  (parameter \"db\") undefined in config file"]),
	{error, undefined_db};
connect(Host, User, Db, Pass, _Opts) ->
	Params = [{host, Host}, {user, User}, {password, Pass}, {database, Db}],
	process_flag(trap_exit, true),
	mysql:start_link(Params).

query({_Cnf, Con}, Query, Fun) when is_function(Fun, 0) ->
	Ret = mysql:transaction(Con, fun() -> 
		ok = result(mysql:query(Con, Query)), Fun() end), 
	Ret.

result({aborted, Reason}) -> 
	pgup_log:msg("~s", cake:fg(lightred, Reason)),
	{error, Reason};
result({error, {_Num, _Code, Reason}}) ->
	pgup_log:msg("SQL Error: ~s", [cake:fg(lightred, reason_to_str(Reason))]),
	{error, Reason};
result(ok) -> ok;
result({atomic, ok}) ->
	ok;
result([H|T]) ->
	ok = result(H),
	result(T);
result({ok, [], []}) ->
	ok.

reason_to_str(R) when is_binary(R) ->
	binary_to_list(R).

init_db_schema({_Cnf, Con}) ->
	init_schema_result(mysql:query(Con, "CREATE TABLE _pgup (id integer NOT NULL, ctime DATETIME, CONSTRAINT _pgup_pkey PRIMARY KEY (id));")).

init_schema_result(ok) ->
	ok;
init_schema_result({error, {1050, <<"42S01">>, Text}}) ->
	pgup_log:msg("Schema already initialized (~s)", [binary_to_list(Text)]),
	ok.	


current_version({_Cnf, Con}) ->
	{ok, _, [[Ver]]} = mysql:query(Con, "select max(id) from _pgup"),
	integer(Ver).

integer(null) -> 0;
integer(I) when is_integer(I) -> I.

record_version( {_Cnf, Con}, Version ) ->
	ok = mysql:query(Con, "insert into _pgup (id, ctime) values (?, now())", [Version]),
	ok.

remove_version( {_Cnf, Con}, Version ) ->
	ok = mysql:query(Con, "delete from _pgup where id = ?", [Version]),
	ok.	
