-module(pgup).
-export([main/1]).

main([File, Command]) when is_list(File) andalso is_list(Command) ->
	io:format("Config File: ~p~nCommand: ~p~n", [File, Command]),
	command(Command, process(file:consult(File)));

main(_) ->
	io:format("usage: pgup <config.file> command~nWhere command:~n 	init = init schema~n").

command(_, error) ->
	error;
command("init", {ok, Config}) ->
	io:format("	Init schema~n"),
	init_schema(Config),
	ok;
command("upgrade", {ok, Config}) ->
	Dir = get_dir(Config),
	io:format("	Enumeration dir ~p~n", [Dir]),
	upgrade(file:list_dir(Dir), Config),
	ok.

get_dir(Config) ->
	proplists:get_value(dir, Config, "sql").


upgrade({error, enoent}, _Config) ->
	io:format("	Cannot enumerate files in directory~n"),
	error;
upgrade({ok, []}, _Config) ->
	io:format("Directory is empty~n"),
	error;
upgrade({ok, Files}, Config) ->
	io:format(" Files: ~p~n", [Files]),
	process_files(up, lists:sort(Files), Config).

process_files(_, [], _Config) ->
	io:format("done~n"),
	ok;
process_files(Op, [H|T], Config) ->
	io:format("	Processing ~p file~n", [H]),
	ok = handle_file(Op, H, Config),
	process_files(Op, T, Config).


do_handle_file(up, [_Version, _Name, "down", "sql"], _File, _Config) ->
	ok;
do_handle_file(up, [Version, Name, "up", "sql"], File, Config) ->
	handle_upgrade(Version, Name, File, Config);
do_handle_file(_, _, File, _Config) ->
	io:format("Don`t know what to do with file ~p, correct file name format: version.description.[up|down].sql~n", [File]),
	ok.

handle_upgrade(Version, Name, File, Config) ->
	io:format("	processing upgrade ~p ~p~n", [Version, Name]),
	{ok, C} = connect(Config),
	upgrade_result(file_squery(C, file_get_content(filename:join(get_dir(Config), File))), C).

file_squery(C, {ok, B}) when is_binary(B) ->
	epgsql:squery(C, binary_to_list(B));
file_squery(_C, A) ->
	io:format("Error reading file, ~p ~n", [A]),
	error.

file_get_content(File) ->
	io:format("Reading file content ~p~n", [File]),
	file:read_file(File).

upgrade_result(A, _) ->
	io:format("Upgrade result ~p ~n", [A]),
	ok.

handle_file(Op, File, Config) ->
	do_handle_file(Op,string:tokens(File, "."),  File, Config).

init_schema(Config) ->
	{ok, C} = connect(Config),
	init_schema_result(epgsql:squery(C, "CREATE TABLE _pgup (id character varying(255) NOT NULL, ctime timestamp without time zone, CONSTRAINT _pgup_pkey PRIMARY KEY (id)) WITH (OIDS=FALSE);")).

init_schema_result({ok, [], []}) ->
	ok;
init_schema_result({error, {error, error, <<"42P07">>, Text, _}}) ->
	io:format("Schema already initialized (~s)~n", [binary_to_list(Text)]),
	ok.	

process({error, enoent}) ->
	io:format("Config file not found.~n"),
	error;
process({ok, [Config]}) ->
	io:format("Config: ~p~n", [Config]),
	check_config(Config);
process(A) ->
	io:format("Error reading config file: ~p~n", [A]),
	error.

connect(Cfg) -> 
	Host = proplists:get_value(host, Cfg, "127.0.0.1"),
	User = proplists:get_value(user, Cfg, "postgres"),
	Db	 = proplists:get_value(db, Cfg),
	Pass = proplists:get_value(pass, Cfg, ""),
	Opts = proplists:get_value(opts, Cfg, []),
	io:format("Connecting to ~p with user ~p and db ~p...~n", [Host, User, Db]),
	connect(Host, User, Db, Pass, Opts).

connect(_, _, undefined, _, _ ) ->
	io:format("	Database name  (parameter \"db\") undefined in config file~n"),
	error;
connect(Host, User, Db, Pass, Opts) ->
	epgsql:connect(Host, User, Pass, [ {database, Db} | Opts ] ).

check_config(Cfg) ->
	check_result(connect(Cfg), Cfg).

check_result(error, _) ->
	error;
check_result({error, invalid_password}, _) ->
	io:format("	Error: Password is invalid~n"),
	error;
check_result({ok, C}, Cfg) when is_pid(C) ->
	{ok, Cfg}.