-module(examples_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ServerConfig = {server_config, default, [
				{host, '_'},
				{protocol, http},
				{port, 8080},
				{acceptor_number, 100},
				{max_connections, infinity}
				]},
	{ok, ServerID} = kill_bill:config_server(ServerConfig),
	
	{ok, AppName} = application:get_application(),
	
	RootWebApp = {webapp_config, root, [
				{context, "/"},
				{template, [
						{top_page, "hello_world"}
						]},
				{resource, [
						{base_name, "message"},
						{file_extension, ".txt"},  
						{priv_dir, AppName, "resource"}
						]},
				{static, [
						{path, "/"}, 
						{priv_dir, AppName, "static"}
						]}
				]},
	ok = kill_bill:deploy(ServerID, RootWebApp),
	
	ExamplesWebApp = {webapp_config, examples, [
				{context, "/examples/"},
				{template, [
						{top_page, "index"}, 
						{prefix, "page"}
						]},
				{resource, [
						{base_name, "message"},
						{file_extension, ".txt"},  
						{priv_dir, AppName, "resource"}
						]},
				{action, [ 
						{examples_filter, [
								{"action", examples_action}
								]}		
						]},
				{static, [
						{path, "/"}, 
						{priv_dir, AppName, "static"}
						]}
				]},
		ok = kill_bill:deploy(ServerID, ExamplesWebApp),
	
	ok = kill_bill:start_server(ServerID),
	
	examples_sup:start_link().

stop(_State) ->
	ok.