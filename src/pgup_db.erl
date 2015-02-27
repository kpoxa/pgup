-module(pgup_db).
-export([connect/1, query/3, create_db/1, init_db_schema/1, current_version/1, record_version/2, remove_version/2]).

-define(ENGINES, [{pg, pgup_epgsql}, {mysql, pgup_mysql}]).

connect(Config) ->
	run(Config, connect, [Config]).

create_db(Config) ->
	Db = proplists:get_value(db, Config),
	Master = run(Config, master_db, []),
	run(Config, create_db, [connect(change(Config, db, Master)), Db]).

change(Proplist, Key, Value) ->
	lists:keystore(Key, 1, Proplist, {Key, Value}).	

init_db_schema(Config) ->
	pgup_log:msg("Config = ~p", [Config]),
	run(Config, init_db_schema, [connect(Config)]).

query(C = {Config, _Con}, Query, F) when is_function(F, 0) ->
	run(Config, query, [C, Query, F]).

current_version(C = {Config, _Con}) ->
	integer(run(Config, current_version, [C])).

record_version(C = {Config, _Con}, Version) when is_integer(Version) ->
	run(Config, record_version, [C, Version]).

remove_version(C = {Config, _Con}, Version) when is_integer(Version) ->
	run(Config, remove_version, [C, Version]).

run(Config, F, A) ->
	Module = get_module(Config),
	erlang:apply(Module, F, A).

get_module(Config) ->
	Module = proplists:get_value(proplists:get_value(engine, Config, pg), ?ENGINES),
	Module.

integer(I) when is_integer(I) ->
	I;
integer(null) ->
	0.