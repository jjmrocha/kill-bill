-module(hello_world_app).

-behaviour(application).

-include("../deps/kill_bill/include/kill_bill.hlr").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Template = #template_config{},
	Resource = #resource_config{},
	Action = [#action_config{callback=hello_world_action}],
	Static = #static_config{},
	WebApp = #web_app{template=Template, resource=Resource, action=Action, static=Static},
	
	kill_bill:deploy(hello_world, hello_world_webapp, WebApp),

    hello_world_sup:start_link().

stop(_State) ->
	ok.