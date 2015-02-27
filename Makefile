PROJECT = pgup
DEPS = epgsql mysql cake

dep_mysql = git https://github.com/mysql-otp/mysql-otp 0.8.1

COMPILE_FIRST = pgup_db_behaviour.erl 

include erlang.mk
