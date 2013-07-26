-module(examples_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ok = kill_bill:config_server("./priv/default-server.config"),
	ok = kill_bill:deploy(default, "./priv/root-webapp.config"),
	ok = kill_bill:deploy(default, "./priv/examples-webapp.config"),
	ok = kill_bill:start_server(default),
	
	examples_sup:start_link().

stop(_State) ->
	ok.