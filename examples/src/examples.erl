-module(examples).

-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(syntax_tools),
	ok = application:start(compiler),
	ok = application:start(ranch),
	ok = application:start(columbo),
	ok = application:start(cclock),
	ok = application:start(gibreel),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(jsondoc),
	ok = application:start(erlydtl),
	ok = application:start(kill_bill),
	ok = application:start(examples).

