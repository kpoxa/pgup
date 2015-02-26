-module(pgup_epgsql).
-behaviour (pgup_db_behaviour).
-export([query/3, connect/1, init_db_schema/1, current_version/1, record_version/2, remove_version/2]).

connect(Cfg) -> 
	Host = proplists:get_value(host, Cfg, "127.0.0.1"),
	User = proplists:get_value(user, Cfg, "postgres"),
	Db	 = proplists:get_value(db, Cfg),
	Pass = proplists:get_value(pass, Cfg, ""),
	Opts = proplists:get_value(opts, Cfg, []),
	pgup_log:msg(["Connecting to ~p with user ~p and db ~p..."], [Host, User, Db]),
	Pid = connection_result(connect(Host, User, Db, Pass, Opts)),
	{Cfg, Pid}.

connection_result({ok, Pid}) when is_pid(Pid) ->
	Pid;
connection_result({error, invalid_password}) ->
	pgup_log:msg("~s", [cake:fg(lightred, "Invalid password or username")]),
	throw(invalid_password).
	
connect(_, _, undefined, _, _ ) ->
	pgup_log:msg(["	Database name  (parameter \"db\") undefined in config file"]),
	{error, undefined_db};
connect(Host, User, Db, Pass, Opts) ->
	epgsql:connect(Host, User, Pass, [ {database, Db} | Opts ] ).

query({_Cnf, Con}, Query, Fun) when is_function(Fun, 0) ->
	result(epgsql:with_transaction(Con, fun(Conn) -> 
		epgsql:squery(Conn, Query), Fun() end )).	

result([]) -> 
	ok;
result(ok) ->
	ok;
result([H|T]) ->
	ok = result(H),
	result(T);
result({ok, [], []}) ->
	ok.

init_db_schema({_Cnf, Con}) ->
	init_schema_result(epgsql:squery(Con, "CREATE TABLE _pgup (id integer NOT NULL, ctime timestamp without time zone, CONSTRAINT _pgup_pkey PRIMARY KEY (id)) WITH (OIDS=FALSE);")).

init_schema_result({ok, [], []}) ->
	ok;
init_schema_result({error, {error, error, <<"42P07">>, Text, _}}) ->
	pgup_log:msg("Schema already initialized (~s)", [binary_to_list(Text)]),
	ok.	


current_version({_Cnf, Con}) ->
	{ok, _, [{Ver}]} = epgsql:equery(Con, "select max(id) from _pgup"),
	Ver.

record_version( {_Cnf, Con}, Version ) ->
	{ok, 1} = epgsql:equery(Con, "insert into _pgup (id, ctime) values ($1, current_timestamp)", [Version]),
	ok.

remove_version( {_Cnf, Con}, Version ) ->
	{ok, 1} = epgsql:equery(Con, "delete from _pgup where id = $1", [Version]),
	ok.	
