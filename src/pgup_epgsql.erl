-module(pgup_epgsql).
-behaviour (pgup_db_behaviour).
-export([query/3, connect/1, init_db_schema/1, current_version/1, record_version/2, remove_version/2, master_db/0, create_db/2]).

master_db() ->
	"postgres".

connect(Cfg) -> 
	Host = proplists:get_value(host, Cfg, "127.0.0.1"),
	User = proplists:get_value(user, Cfg, "postgres"),
	Db	 = proplists:get_value(db, Cfg),
	Pass = proplists:get_value(pass, Cfg, ""),
	Opts = proplists:get_value(opts, Cfg, []),
	pgup_log:msg("Connecting to ~p with user ~p and db ~p...", [Host, User, Db]),
	Pid = connection_result(connect(Host, User, Db, Pass, Opts)),
	{Cfg, Pid}.

create_db( { _Cnf, Con }, Db ) ->
	result(epgsql:equery(Con, "CREATE DATABASE " ++ to_string(Db))).

to_string(Db) when is_atom(Db) -> atom_to_list(Db);
to_string(Db) when is_binary(Db) -> binary_to_list(Db);
to_string(Db) when is_list(Db) -> Db.

connection_result({ok, Pid}) when is_pid(Pid) ->
	Pid;
connection_result(E = {error, {error, _, _Code, _Reason, _}}) ->
	result(E);
connection_result({error, invalid_password}) ->
	pgup_log:err("Invalid password or username"),
	throw(invalid_password).
	
connect(_, _, undefined, _, _ ) ->
	pgup_log:err("Database name  (parameter \"db\") undefined in config file"),
	{error, undefined_db};
connect(Host, User, Db, Pass, Opts) ->
	epgsql:connect(Host, User, Pass, [ {database, Db} | Opts ] ).

query({_Cnf, Con}, Query, Fun) when is_function(Fun, 0) ->
	result(epgsql:with_transaction(Con, fun(Conn) -> 
		epgsql:squery(Conn, Query), Fun() end )).	

result([]) -> 
	ok;
result({error, {error, _, Code, Reason, _}}) ->
	pgup_log:err("Sql error ~s: ~s", [Code, Reason]),
	throw(sql_error);
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
	pgup_log:msg("Schema already initialized (~s)", [to_string(Text)]),
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
