-module(pgup_command).
-export([upgrade/1, downgrade/2]).

upgrade(Config) ->
	C = pgup_db:connect(Config),
	CurrentVersion = current(C),
	Files = pgup_files:upgrade_scripts(Config, CurrentVersion),
	execute_upgrades(lists:keysort(1, Files), C).

execute_upgrades([], _) -> 
	ok();
execute_upgrades([H|T], C) ->
	ok = execute_upgrade(H, C),
	execute_upgrades(T, C).

execute_upgrade({Num, Sql}, C) ->
	pgup_log:msg("	process update #~p...", [Num]),
	ok = pgup_db:query(C, Sql, fun() -> ok = pgup_db:record_version(C, Num) end),
	pgup_log:ok(" ...done"),
	ok.

current(C) ->
	CurrentVersion = pgup_db:current_version(C),
	pgup_log:ok("current database version: ~B", [CurrentVersion]),
	CurrentVersion.

downgrade(Config, ToVersion) ->
	C = pgup_db:connect(Config),
	CurrentVersion = current(C),
	Files = pgup_files:downgrade_scripts(Config, CurrentVersion, ToVersion),
	execute_downgrades(lists:reverse(lists:keysort(1, Files)), C).

execute_downgrades([], _) -> 
	ok();
execute_downgrades([H|T], C) ->
	ok = execute_downgrade(H, C),
	execute_downgrades(T, C).

execute_downgrade({Num, Sql}, C) ->
	pgup_log:msg("	process downgrade #~p...", [Num]),
	ok = pgup_db:query(C, Sql, fun() -> ok = pgup_db:remove_version(C, Num) end),
	pgup_log:ok(" ...done"),
	ok.

ok() ->
	pgup_log:ok("Everything ok"),
	ok.
