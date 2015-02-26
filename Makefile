PROJECT = pgup
DEPS = epgsql cake

COMPILE_FIRST = pgup_db_behaviour.erl 

include erlang.mk
