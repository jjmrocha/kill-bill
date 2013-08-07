-module(examples).

-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(columbo),
	ok = application:start(gibreel),
	ok = application:start(cowboy),
	ok = application:start(kill_bill),
	ok = application:start(examples).

