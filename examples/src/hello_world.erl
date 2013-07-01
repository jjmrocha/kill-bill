-module(hello_world).

-export([start/0]).

start() ->
	ok = application:start(kill_bill),
	ok = application:start(hello_world).

