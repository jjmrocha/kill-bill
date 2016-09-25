%%
%% Copyright 2013-16 Joaquim Rocha
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

-behaviour(gen_server).

-define(PROTOCOL_HTTP, http).
-define(PROTOCOL_HTTPS, https).

-define(SERVER, {local, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([config_server/1, start_server/1, stop_server/1, get_server_list/0]).
-export([deploy/2, undeploy/1, get_webapp_list/0]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

-spec config_server(ServerConfig :: Config) -> {ok, ServerName :: atom()} | {error, Reason :: any()} 
	when Config :: ConfigFile :: string()
	| {server_config, ServerName :: atom(), Config :: list()}.
config_server({server_config, ServerName, Config}) when is_atom(ServerName) andalso is_list(Config) ->
	gen_server:call(?MODULE, {config_server, ServerName, Config});
config_server(FileName) when is_list(FileName) ->
	case load_configuration(FileName) of
		not_found -> {error, not_found};
		{error, Reason} -> {error, Reason};
		{ok, Config} ->	config_server(Config)
	end.

-spec start_server(ServerName :: atom()) -> ok | {error, Reason :: any()}.
start_server(ServerName) when is_atom(ServerName) ->
	gen_server:call(?MODULE, {start_server, ServerName}).

-spec stop_server(ServerName :: atom()) -> ok | {error, Reason :: any()}.
stop_server(ServerName) when is_atom(ServerName) ->
	gen_server:call(?MODULE, {stop_server, ServerName}).

-spec get_server_list() -> list().
get_server_list() ->
	gen_server:call(?MODULE, {get_server_list}).

-spec deploy(ServerName :: atom(), WebAppConfig :: Config) -> ok | {error, Reason :: any()} 
	when Config :: WebAppFile :: string()
	| {webapp_config, WebAppName :: atom(), Config :: list()}.	  
deploy(ServerName, {webapp_config, WebAppName, Config}) when is_atom(ServerName) andalso is_atom(WebAppName) andalso is_list(Config) ->
	gen_server:call(?MODULE, {deploy, ServerName, {WebAppName, Config}});
deploy(ServerName, FileName) when is_atom(ServerName) andalso is_list(FileName) ->
	case load_configuration(FileName) of
		not_found -> {error, not_found};
		{error, Reason} -> {error, Reason};
		{ok, Config} -> deploy(ServerName, Config)
	end.

-spec undeploy(WebAppName :: atom()) -> ok | {error, Reason :: any()}.
undeploy(WebAppName) when is_atom(WebAppName) ->
	gen_server:call(?MODULE, {undeploy, WebAppName}).

-spec get_webapp_list() -> list().
get_webapp_list() ->
	gen_server:call(?MODULE, {get_webapp_list}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

-record(server, {config, webapps = [], running=false}).
-record(webapp, {config, resource, server, session}).
-record(status, {servers, webapps}).

init([]) ->
	process_flag(trap_exit, true),	
	error_logger:info_msg("Bang Bang, ~p [~p] Starting...\n", [?MODULE, self()]),
	{ok, #status{servers=dict:new(), webapps=dict:new()}}.

handle_call({config_server, ServerName, Config}, _From, State=#status{servers=Servers}) ->
	Reply = case dict:find(ServerName, Servers)  of
		error ->						
			NServers = dict:store(ServerName, #server{config=Config}, Servers),
			NState = State#status{servers=NServers},
			error_logger:info_msg("Server ~p configured: ~p\n", [ServerName, Config]),
			{ok, ServerName};
		{ok, #server{running=false, webapps=WebApps}} ->
			NServers = dict:store(ServerName, #server{config=Config, webapps=WebApps}, Servers),
			NState = State#status{servers=NServers},
			error_logger:info_msg("Server ~p reconfigured: ~p\n", [ServerName, Config]),
			{ok, ServerName};	
		{ok, _Server} ->
			error_logger:error_msg("KB: Server ~p is running\n", [ServerName]),
			NState = State,
			{error, server_running}
	end,
	{reply, Reply, NState};
handle_call({start_server, ServerName}, _From, State=#status{servers=Servers, webapps=Webapps}) ->
	Reply = case dict:find(ServerName, Servers)  of
		error ->			
			error_logger:error_msg("KB: Server ~p not found\n", [ServerName]),
			NState = State,
			{error, not_found};
		{ok, #server{running=true}} ->
			error_logger:info_msg("Server ~p was already running!\n", [ServerName]),
			NState = State,
			ok;
		{ok, Server} ->
			Protocol = proplists:get_value(protocol, Server#server.config, ?PROTOCOL_HTTP),
			NbAcceptors = proplists:get_value(acceptor_number, Server#server.config, 100),
			Host = get_host(Server#server.config),
			Port = get_port(Server#server.config),
			
			PathsList = get_server_paths(ServerName, Host, Server, Webapps),
			
			Dispatch = cowboy_router:compile([{Host, PathsList}]),
			ProtoOpts = [{env, [{dispatch, Dispatch}]}],
			
			TransOpts = get_server_config(Protocol, Port, Server#server.config),
			
			case Protocol of
				?PROTOCOL_HTTP -> 
					{ok, _} = cowboy:start_http(ServerName, NbAcceptors, TransOpts, ProtoOpts);
				?PROTOCOL_HTTPS -> 
					{ok, _} = cowboy:start_https(ServerName, NbAcceptors, TransOpts, ProtoOpts)
			end,
			
			error_logger:info_msg("Server ~p started!\n", [ServerName]),
			
			NServers = dict:store(ServerName, Server#server{running=true}, Servers),
			NState = State#status{servers=NServers},
			ok
	end,
	{reply, Reply, NState};
handle_call({stop_server, ServerName}, _From, State=#status{servers=Servers}) ->
	Reply = case dict:find(ServerName, Servers)  of
		error ->			
			error_logger:error_msg("KB: Server ~p not found\n", [ServerName]),
			NState = State,
			{error, not_found};
		{ok, #server{running=false}} ->
			error_logger:info_msg("Server ~p was not running!\n", [ServerName]),
			NState = State,
			ok;
		{ok, Server} ->
			cowboy:stop_listener(ServerName),
			error_logger:info_msg("Server ~p stoped!\n", [ServerName]),
			
			NServers = dict:store(ServerName, Server#server{running=false}, Servers),
			NState = State#status{servers=NServers},
			ok
	end,
	{reply, Reply, NState};
handle_call({get_server_list}, From, State=#status{servers=Servers}) ->
	Fun = fun() ->
			Rep = fun(ServerName, #server{running=Run, webapps=WebApps}, Acc) ->
					[{ServerName, Run, WebApps} | Acc]
			end,
			Reply = dict:fold(Rep, [], Servers),
			gen_server:reply(From, Reply)
	end,
	spawn(Fun),
	{noreply, State};
handle_call({deploy, ServerName, {WebAppName, Config}}, _From, State=#status{servers=Servers, webapps=Webapps}) ->
	Reply = case dict:find(WebAppName, Webapps) of
		error ->
			case dict:find(ServerName, Servers) of
				error ->
					error_logger:error_msg("KB: Server ~p not configured\n", [ServerName]),
					NState = State,
					{error, not_found};
				{ok, Server} ->
					Resource = create_resource_server(Config),
					SessionManager = create_session(WebAppName, Config),					
					WebApp = #webapp{config=Config, resource=Resource, server=ServerName, session=SessionManager},
					NWebapps = dict:store(WebAppName, WebApp, Webapps),
					
					NServer = Server#server{webapps=[WebAppName | Server#server.webapps]},
					NServers = dict:store(ServerName, NServer, Servers),
					
					case NServer#server.running of
						true ->
							Host = get_host(NServer#server.config),
							PathsList = get_server_paths(ServerName, Host, NServer, NWebapps),
							Dispatch = cowboy_router:compile([{Host, PathsList}]),
							cowboy:set_env(ServerName, dispatch, Dispatch);
						false -> ok
					end,
					
					error_logger:info_msg("WebApp ~p deployed on server ~p, with Config: ~p\n", [WebAppName, ServerName, Config]),
					NState = State#status{servers=NServers, webapps=NWebapps},
					ok
			end;
		{ok, _App} ->
			error_logger:error_msg("KB: Duplicated webapp ~p\n", [WebAppName]),
			NState = State,
			{error, duplicated}
	end,
	{reply, Reply, NState};
handle_call({undeploy, WebAppName}, _From, State=#status{servers=Servers, webapps=Webapps}) ->
	Reply = case dict:find(WebAppName, Webapps) of
		error ->			
			error_logger:error_msg("KB: WebApp ~p not found\n", [WebAppName]),
			NState = State,
			{error, not_found};
		{ok, WebApp} ->
			NWebapps = dict:erase(WebAppName, Webapps),
			{ok, Server} = dict:find(WebApp#webapp.server, Servers),
			NServer = Server#server{webapps=lists:delete(WebAppName, Server#server.webapps)},
			NServers = dict:store(WebApp#webapp.server, NServer, Servers),
			
			case NServer#server.running of
				true ->
					Host = get_host(NServer#server.config),
					PathsList = get_server_paths(WebApp#webapp.server, Host, NServer, NWebapps),
					
					Dispatch = cowboy_router:compile([{Host, PathsList}]),
					cowboy:set_env(WebApp#webapp.server, dispatch, Dispatch);
				false -> ok
			end,
			
			stop_resource_server(WebApp#webapp.resource),
			stop_session(WebApp#webapp.session),
			
			error_logger:info_msg("WebApp ~p was undeployed from server ~p\n", [WebAppName, WebApp#webapp.server]),
			NState = State#status{servers=NServers, webapps=NWebapps},
			ok
	end,
	{reply, Reply, NState};
handle_call({get_webapp_list}, From, State=#status{webapps=Webapps}) ->
	Fun = fun() ->
			Rep = fun(WebAppName, #webapp{server=ServerName}, Acc) ->
					[{WebAppName, ServerName} | Acc]
			end,
			Reply = dict:fold(Rep, [], Webapps),
			gen_server:reply(From, Reply)
	end,
	spawn(Fun),
	{noreply, State}.

handle_cast(Msg, State) ->
	error_logger:info_msg("handle_cast(~p)\n", [Msg]),
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

load_configuration(FileName) ->
	case filelib:is_file(FileName) of
		true ->	file:script(FileName);
		false -> not_found
	end.

get_host(ServerConfig) ->
	proplists:get_value(host, ServerConfig, '_').

get_port(ServerConfig) ->
	proplists:get_value(port, ServerConfig, 8080).

get_server_config(Protocol, Port, ServerConfig) ->
	MaxCon = proplists:get_value(max_connections, ServerConfig, infinity),
	SslConfig = proplists:get_value(ssl, ServerConfig),
	Ssl = get_ssl(Protocol, SslConfig),
	lists:append([{port, Port}, {max_connections, MaxCon}], Ssl).

get_ssl(?PROTOCOL_HTTP, _Ssl) -> [];
get_ssl(_Protocol, none) -> [];
get_ssl(_Protocol, Ssl) -> 
	[
		{cacertfile, proplists:get_value(cacertfile, Ssl)},
		{certfile, proplists:get_value(certfile, Ssl)},
		{keyfile, proplists:get_value(keyfile, Ssl)}
		].

create_resource_server(WebAppConfig) ->
	ResourceConfig = proplists:get_value(resource, WebAppConfig, none),
	get_resource_server(ResourceConfig).

get_resource_server(none) -> none;
get_resource_server(ResourceConfig) ->
	{ok, Pid} = kb_resource_sup:start_resource_server(ResourceConfig),
	Pid.

stop_resource_server(none) -> ok;
stop_resource_server(Pid) -> kb_resource:stop(Pid).

create_session(WebAppName, WebAppConfig) ->
	case proplists:get_value(session_timeout, WebAppConfig, 30) of
		none -> none;
		SessionTimeout  ->
			SessionCache = list_to_atom(atom_to_list(WebAppName) ++ "_session"),
			Options = [{max_age, SessionTimeout * 60},
					{purge_interval, 60},
					{cluster_nodes, all},
					{sync_mode, full}],
			gibreel:create_cache(SessionCache, Options),
			SessionCache
	end.

stop_session(none) -> ok;
stop_session(SessionCache) ->
	gibreel:delete_cache(SessionCache).

get_server_paths(ServerName, Host, Server, Webapps) ->
	case Server#server.webapps of 
		[] -> 
			[{'_', kb_dummy_toppage, [{server, ServerName}, {host, Host}]}];
		WAList -> 
			AppList = get_app_list(WAList, Webapps, []),
			get_web_app_config(AppList, [])
	end.

get_app_list([], _Webapps, AppList) ->
	Fun = fun({_, A, _}, {_, B, _}) -> A > B end,
	lists:sort(Fun, AppList);
get_app_list([WebAppName|T], Webapps, AppList) ->
	{ok, WebApp} = dict:find(WebAppName, Webapps),
	Context = get_context(proplists:get_value(context, WebApp#webapp.config, "/")),
	get_app_list(T, Webapps, [{WebAppName, Context, WebApp} | AppList]).

remove_slashs(Path) ->
	kb_util:remove_if_ends_with(kb_util:remove_if_starts_with(Path, "/"), "/").

get_web_app_config([], Paths) -> lists:reverse(Paths);
get_web_app_config([{_WebAppName, Context, WebApp} | T], Paths) ->
	ResourceServer = WebApp#webapp.resource,
	SessionManager = WebApp#webapp.session,
	TemplateConfig = proplists:get_value(template, WebApp#webapp.config, none),
	ActionConfig = proplists:get_value(action, WebApp#webapp.config, []),
	StaticConfig = proplists:get_value(static, WebApp#webapp.config, none),
	Static = get_static(StaticConfig),
	Options = [
			{resource_server, ResourceServer},
			{context, Context},
			{session_manager, SessionManager},
			{static, Static}
			],
	ActionPath = action_path(ActionConfig, [], []),
	PathsWithTemplate = add_template(TemplateConfig, Context, Options, []),
	PathsWithAction = add_action(ActionPath, Context, Options, PathsWithTemplate),
	PathsWithStatic = add_static(StaticConfig, Context, PathsWithAction),
	Sorted = lists:sort(fun({A, _, _}, {B, _, _}) -> sort_paths(A, B) end, PathsWithStatic),
	get_web_app_config(T, lists:append(Sorted, Paths)).

get_static(none) -> "";
get_static(Config) ->
	Path = proplists:get_value(path, Config, "/"), 
	remove_slashs(Path) ++ "/".

action_path([{Filter, SubConfig}|T], Path, Output) when is_atom(Filter) andalso is_list(SubConfig) ->
	NewOutput = action_path(SubConfig, [Filter|Path], []),
	action_path(T, Path, Output ++ NewOutput);
action_path([{ActionContext, Action}|T], Path, Output) ->
	CallbackList = lists:reverse([Action|Path]),
	action_path(T, Path, [{ActionContext, CallbackList}|Output]);
action_path([], _Path, Output) -> Output.

sort_paths(A, B) ->
	{UA, QA} = fix_path(A),
	{UB, QB} = fix_path(B),
	if UA < UB -> true;
		UA == UB -> QA > QB;
		true -> false
	end.

fix_path(Path) ->
	case string:rstr(Path, "[...]") of
		0 -> {Path, ""};
		Len -> 
			Url = string:substr(Path, 1, Len - 1),
			{Url, "[...]"}
	end.

add_template(none, _Context, _Options, Paths) -> Paths;
add_template(TemplateConfig, Context, Options, Paths) ->
	TemplatePrefix = proplists:get_value(prefix, TemplateConfig, "page"),
	TopPage = proplists:get_value(top_page, TemplateConfig, "index"),
	NPaths = add_top_page(TopPage, Context, Options, Paths),
	lists:append([
			{get_template_match(TemplatePrefix, Context), kb_cowboy_template, Options}
			], NPaths).

add_top_page(none, _Context, _Options, Paths) -> Paths;
add_top_page(TopPage, Context, Options, Paths) -> 
	lists:append([
			{Context, kb_cowboy_toppage, [{top_page, TopPage}] ++ Options}
			], Paths).	

get_template_match(TemplatePrefix, Context) ->
	Context ++ remove_slashs(TemplatePrefix) ++ "/[...]".

add_action([], _Context, _Options, Paths) -> Paths;
add_action([{ActionPrefix, CallbackList}|T], Context, Options, Paths) ->
	NakedActionPrefix = remove_slashs(ActionPrefix),
	NPaths = lists:append([
				{get_action_match(NakedActionPrefix, Context), kb_cowboy_action, [{callback_list, CallbackList}, {action_prefix, NakedActionPrefix}] ++ Options}
				], Paths),
	add_action(T, Context, Options, NPaths).

get_action_match(ActionPrefix, Context) ->
	Context ++ ActionPrefix ++ "/[...]".

add_static(none, _Context, Paths) -> Paths;
add_static(StaticConfig, Context, Paths) ->
	Path = proplists:get_value(path, StaticConfig, "/"), 
	Options = [{mimetypes, cow_mimetypes, all}],
	Config = case lists:keyfind(priv_dir, 1, StaticConfig) of
		{_, App, Dir} -> {priv_dir, App, Dir, Options};
		false -> 
			Dir = proplists:get_value(dir, StaticConfig, "./static"),
			{dir, Dir, Options}
	end,	
	lists:append([
			{get_static_match(Path, Context), cowboy_static, Config}
			], Paths).

get_static_match(Path, Context) ->
	case Path of
		"/" -> string:concat(Context, "[...]");
		_ -> Context ++ remove_slashs(Path) ++ "/[...]"
	end.

get_context("/") -> "/";
get_context(WebContext) -> 
	"/" ++ remove_slashs(WebContext) ++ "/".