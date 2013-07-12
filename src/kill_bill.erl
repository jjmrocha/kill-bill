%%
%% Copyright 2013 Joaquim Rocha
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

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
			
			Dispatch = cowboy_router:compile([{Server#server_config.host, Config}]),
			
			NbAcceptors = Server#server_config.acceptor_number,
			TransOpts = get_server_config(Server),
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
	error_logger:info_msg("Bang Bang, My Baby Shot Me Down\n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_web_app_config(WebApp, App) ->
	Context = get_context(WebApp#web_app.context),
	ResourceServer = get_resource_server(WebApp#web_app.resource),
	Template = add_template(WebApp#web_app.template, Context, ResourceServer, []),
	Action = add_action(WebApp#web_app.action, Context, ResourceServer, Template),
	WebSocket = add_websocket(WebApp#web_app.websocket, Context, App, Action) ,
	Static = add_static(WebApp#web_app.static, Context, WebSocket),
	lists:reverse(Static).

get_context(Context) ->
	case remove_slashs(Context) of
		[] -> "/";
		Clean -> "/" ++ Clean ++ "/"
	end.

get_resource_server(none) -> none;
get_resource_server(Resource) ->
	{ok, Server} = kb_resource_sup:start_resource_server(Resource),
	Server.

add_template(none, _Context, _ResourceServer, Config) -> Config;
add_template(Template, Context, ResourceServer, Config) ->
	lists:append([
			{Context, kb_cowboy_toppage, [
					{resource_server, ResourceServer}, 
					{template_config, Template},
					{context, Context}
					]},
			{get_template_match(Template, Context), kb_cowboy_template, [
					{resource_server, ResourceServer},
					{context, Context}
					]}
			], Config).

add_action([], _ResourceServer, _Context, Config) -> Config;
add_action([Action|T], Context, ResourceServer, Config) ->
	NewConfig = lists:append([
				{get_action_match(Action, Context), get_action_handler(Action#action_config.type), [
						{resource_server, ResourceServer}, 
						{action_config, Action},
						{context, Context}
						]}
				], Config),
	add_action(T, Context, ResourceServer, NewConfig).

get_action_handler(?ACTION_TYPE_BASIC) -> kb_cowboy_action_basic;
get_action_handler(?ACTION_TYPE_FULL) -> kb_cowboy_action_full.

add_websocket(none, _Context, _App, Config) -> Config;
add_websocket(WebSocket, Context, App, Config) ->
	lists:append([
			{string:concat(Context, remove_slashs(WebSocket#websocket_config.path)), bullet_handler, [
					{web_app, App},
					{handler, kb_bullet_websocket}
					]}
			], Config).

add_static(none, _Context, Config) -> Config;
add_static(Static, Context, Config) ->
	lists:append([
			{get_static_match(Static, Context), cowboy_static, [
					{directory, get_static_dir(Static)},
					{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
					]}
			], Config).

get_action_match(Action, Context) ->
	Context ++ remove_slashs(Action#action_config.prefix) ++ "/[...]".

get_template_match(Template, Context) ->
	Context ++ remove_slashs(Template#template_config.prefix) ++ "/[...]".

get_static_match(Static, Context) ->
	case Static#static_config.path of
		"/" -> string:concat(Context, "[...]");
		Path -> Context ++ remove_slashs(Path) ++ "/[...]"
	end.

get_static_dir(Static) ->
	Static#static_config.dir.

remove_slashs(Path) ->
	kb_util:remove_if_ends_with(kb_util:remove_if_starts_with(Path, "/"), "/").

get_server_config(Server) ->
	lists:append([{port, Server#server_config.port}], get_ssl(Server#server_config.protocol, Server#server_config.ssl)).

get_ssl(?PROTOCOL_HTTP, _Ssl) -> [];
get_ssl(_Protocol, none) -> [];
get_ssl(_Protocol, Ssl) -> 
	[
		{cacertfile, Ssl#ssl_config.cacertfile},
		{certfile, Ssl#ssl_config.certfile},
		{keyfile, Ssl#ssl_config.keyfile}
	].
