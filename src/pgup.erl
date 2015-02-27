-module(pgup).
-export([main/1]).

main([File | Command]) when is_list(File) andalso is_list(Command) ->
	pgup_log:msg(["Config File: ~p", "Command: ~p"], [File, Command]),
	ok = command(Command, read_config(file:consult(File)));

main(_) ->
	pgup_log:msg([
"	Usage:
		pgup <config.file> <command> 
	Commands:
		create					create database
		init 					init schema
		upgrade					upgrade to latest database version
		downgrade X				downgrade to version X
"]).

command(_, error) ->
	error;

command(_, {error, enoent}) ->
	pgup_log:msg("Config file not found."),
	ok;

command(["create"], {ok, Config}) ->
	pgup_log:msg("	Create database..."),
	pgup_db:create_db(Config),
	pgup_log:msg("	...done");

command(["init"], {ok, Config}) ->
	pgup_log:msg("	Init schema..." ),
	pgup_db:init_db_schema(Config),
	pgup_log:msg("	...done");

command(["upgrade"], {ok, Config}) ->
	pgup_command:upgrade(Config),
	ok;

command(["downgrade", Version], {ok, Config}) ->
	Vers = integer(Version),
	pgup_command:downgrade(Config, Vers),
	ok;
command(["downgrade"], _) ->
	pgup_log:msg(["~nUsage: pgup_log <config.file> downgrade <versionNumber>"]),
	ok;
command(Command, _) ->
	pgup_log:msg("~s ~s", [cake:fg(lightred, "Unknown command"), cake:fg(lightred, Command)]),
	ok.

integer(L) when is_list(L) ->
	list_to_integer(L).

read_config(E = {error, enoent}) ->
	E;
read_config({ok, [Config]}) ->
	{ok, Config};
read_config(A) ->
	pgup_log:msg("Error reading config file: ~p", [A]),
	{error, A}.

