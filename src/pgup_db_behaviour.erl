-module(pgup_db_behaviour).

-type proplist() :: list( { atom(), term() } ).
-type connection_info() :: { proplist(), any() }.

-callback connect( Config :: proplist() ) -> connection_info() | {error, Reason :: string()} | error.

-callback query( connection_info(), Query :: string(), F :: fun() ) -> ok.

-callback init_db_schema( connection_info() ) -> ok.

-callback current_version( connection_info() ) -> integer().

-callback record_version( connection_info(), Ver :: integer() ) -> ok.

-callback remove_version( connection_info(), Ver :: integer() ) -> ok.