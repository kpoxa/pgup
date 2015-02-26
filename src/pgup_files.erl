-module(pgup_files).

-export([upgrade_scripts/2, downgrade_scripts/3]).

upgrade_scripts(Config, FromVersion) ->
	[F || F = {Num, _} <- load_files(get_dir(Config), up), Num > FromVersion].

downgrade_scripts(Config, FromVersion, ToVersion) ->
	[F || F = {Num, _} <- load_files(get_dir(Config), down), Num > ToVersion, Num =< FromVersion].

load_files(Dir, Op) ->
	Files = list_files(Dir),
	Meta = files_meta(lists:sort(Files)),
	[{Num, file_get_content(Dir, F)} || [{filename, F}, {version, Num}, {op, O}, _]  <- Meta, O == Op].

list_files(Dir) ->
	list_files_result(file:list_dir_all(Dir)).

list_files_result({error, enoent}) ->
	pgup_log:msg("~s", [cake:fg(lightred, "Can't open directory with sql files or directory is empty")]),
	throw(directory_not_found);
list_files_result({ok, Files}) ->
	Files.

file_get_content(Dir, File) ->
	Path = filename:join([Dir, File]),
	{ok, Ret} = file:read_file(Path),
	check_file_content(Ret).

check_file_content(<<>>) ->
	pgup_log:msg("~s", [cake:fg(lightred, "SQL file is empty")]),
	throw(file_is_empty);
check_file_content(B) when is_binary(B) ->
	B;
check_file_content(A) ->
	throw({problem_with_file, A}).

files_meta(L) ->
	[F || F <- files_meta([], L), is_list(F)].

files_meta(Ret, []) ->
	Ret;
files_meta(L, [H|T]) ->
	files_meta([file_meta(H) | L], T). 

file_meta(Name) ->
	[ {filename, Name} | file_meta_from_tokens(string:tokens(Name, ".")) ].

file_meta_from_tokens([Version, Name, Op, "sql"]) ->
	Ret = [{version, list_to_integer(Version)}, op(Op), {name, Name}],
	Ret;
file_meta_from_tokens(T) ->
	pgup_log:msg("wrong file name tokens ~p, example : 0001.init_tables.up.sql or 0003.drop_indices.down.sql", [T]),
	skip.

op("up") ->
	{op, up};
op("down") ->
	{op, down};
op(Wrong) ->
	pgup_log:msg("Wrong operation in file name: ~p (valid values are \"up\" or \"down\")", [Wrong]),
	throw(wrong_script_filename).

get_dir(Config) ->
	proplists:get_value(dir, Config, "sql").