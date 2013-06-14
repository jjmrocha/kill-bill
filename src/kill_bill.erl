-module(kill_bill).

-include("kill_bill.hlr").

-behaviour(gen_server).

-define(SERVER, {local, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, deploy/3, cast/2, call/2]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

deploy(AppName, Callback, WebApp) when is_atom(AppName) andalso is_atom(Callback) andalso is_record(WebApp, web_app) ->
	gen_server:call(?MODULE, {deploy, AppName, Callback, WebApp}).

cast(AppName, Msg) when is_atom(AppName) ->
	gen_server:cast(?MODULE, {cast, AppName, Msg}).

call(AppName, Msg) when is_atom(AppName) ->
	gen_server:call(?MODULE, {call, AppName, Msg}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
	process_flag(trap_exit, true),
	
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	
	error_logger:info_msg("Bang Bang, ~p [~p] Starting...\n", [?MODULE, self()]),
    {ok, dict:new()}.

handle_call({deploy, AppName, Callback, WebApp}, _From, State) ->
	Reply = case dict:find(AppName, State) of
		error ->
			Server = WebApp#web_app.server,
						
		  	{ok, App} = kb_webapp_sup:start_webapp(Callback),
			NState = dict:store(AppName, App, State),
			Config = get_web_app_config(WebApp, App),
			Dispatch = cowboy_router:compile([{Server#server_config.host, [Config]}]),
			
			NbAcceptors = Server#server_config.acceptor_number,
			TransOpts = get_transport_config(Server),
			ProtoOpts = [{env, [{dispatch, Dispatch}]}],
			
			case Server#server_config.protocol of
				?PROTOCOL_HTTP -> 
					{ok, _} = cowboy:start_http(AppName, NbAcceptors, TransOpts, ProtoOpts);
				?PROTOCOL_HTTPS -> 
					{ok, _} = cowboy:start_https(AppName, NbAcceptors, TransOpts, ProtoOpts)
			end,
			
			error_logger:info_msg("WebApp ~p listening on port [~p], with Config: [~p]\n", [AppName, Server#server_config.port, Config]),
			ok;
		{ok, _App} ->
			error_logger:error_msg("Duplicated name ~p\n", [AppName]),
			NState = State,
			duplicated
    end,
    {reply, Reply, NState};
handle_call({call, AppName, Msg}, From, State) ->
	case dict:find(AppName, State) of
		error ->
			error_logger:error_msg("Receive call from ~p for WebApp ~p, but WebApp is not deployed!\n", [From, AppName]),
			{reply, no_webapp, State};
		{ok, App} ->
			Fun = fun () ->
				Reply = kb_webapp:app_call(App, Msg),
				gen_server:reply(From, Reply)
			end,
			spawn(Fun),
			{noreply, State}
	end.

handle_cast({cast, AppName, Msg}, State) ->
	case dict:find(AppName, State) of
		error ->
			error_logger:error_msg("Receive cast for WebApp ~p, but WebApp is not deployed!\n", [AppName]);		
		{ok, App} ->
			kb_webapp:app_cast(App, Msg)
	end,
    {noreply, State}.

handle_info(Info, State) ->
	error_logger:info_msg("handle_info(~p)\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
	error_logger:info_msg("Bang Bang, My Baby Shot Me Down\n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_web_app_config(WebApp, App) ->
	ResourceServer = get_resource_server(WebApp#web_app.resource),
	Template = add_template(WebApp#web_app.template, ResourceServer, []),
	Action = add_action(WebApp#web_app.action, ResourceServer, Template),
	WebSocket = add_websocket(WebApp#web_app.websocket, App, Action) ,
	Static = add_static(WebApp#web_app.static, WebSocket),
	Static.
			
get_resource_server(none) -> none;
get_resource_server(Resource) ->
	{ok, Server} = kb_resource_sup:start_resource_server(Resource),
	Server.
  
add_template(none, _ResourceServer, Config) -> Config;
add_template(Template, ResourceServer, Config) ->
	lists:append([
		{"/", kb_cowboy_toppage, [ResourceServer, Template]},
		{get_template_match(Template), kb_cowboy_template, [ResourceServer, Template]}
	], Config).

add_action(none, _ResourceServer, Config) -> Config;
add_action(Action, ResourceServer, Config) ->
	lists:append([
		{get_action_match(Action), kb_cowboy_action, [ResourceServer, Action]}
	], Config).

add_websocket(none, _App, Config) -> Config;
add_websocket(_Other, App, Config) ->
	lists:append([
		{"/websocket", kb_websocket, [App]}
	], Config).

add_static(none, Config) -> Config;
add_static(Static, Config) ->
	lists:append([
		{get_static_match(Static), cowboy_static, [
			{directory, get_static_dir(Static)},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		]}
	], Config).

get_action_match(Action) ->
	"/[...]" ++ Action#action_config.extension.

get_template_match(Template) ->
	"/[...]" ++ Template#template_config.extension.

get_static_match(Static) ->
	"/" ++ remove_slashs(Static#static_config.context) ++ "/[...]".

get_static_dir(Static) ->
	Static#static_config.dir.

remove_slashs(Path) ->
	kb_util:remove_if_ends_with(kb_util:remove_if_starts_with(Path, "/"), "/").

get_transport_config(Server) ->
	lists:append([{port, Server#server_config.port}], get_ssl(Server#server_config.protocol, Server#server_config.ssl)).

get_ssl(?PROTOCOL_HTTP, _Ssl) -> [];
get_ssl(_Protocol, none) -> [];
get_ssl(_Protocol, Ssl) -> 
	[
		{cacertfile, Ssl#ssl_config.cacertfile},
		{certfile, Ssl#ssl_config.certfile},
		{keyfile, Ssl#ssl_config.keyfile}
	].