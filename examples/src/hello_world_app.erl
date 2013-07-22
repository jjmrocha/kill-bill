-module(hello_world_app).

-behaviour(application).

-include("../deps/kill_bill/include/kill_bill.hlr").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	WebApp = #web_app{context = "/examples/",
			template = #template_config{}, 
			resource = #resource_config{}, 
			action = [ #action_config{callback=hello_world_action} ], 
			static = #static_config{},
			websocket = #websocket_config{}
			},
	
	kill_bill:deploy(hello_world, hello_world_webapp, WebApp),
	
	hello_world_sup:start_link().

stop(_State) ->
	ok.