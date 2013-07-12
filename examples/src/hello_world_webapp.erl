-module(hello_world_webapp).

-behaviour(kb_webapp_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_init/0,
	handle_client_connect/2,
	handle_client_cast/3,
	handle_client_disconnect/2,
	handle_app_cast/2,
	handle_app_call/2,
	handle_terminate/1]).

handle_init() -> 
	error_logger:info_msg("~p: handle_init()\n", [?MODULE]),
	{ok, dict:new()}.

handle_client_connect(Client, State) -> 
	error_logger:info_msg("~p: handle_client_connect(~p)\n", [?MODULE, Client]),
	{ok, dict:store(Client, Client, State)}.

handle_client_cast(Client, Msg, State) ->
	error_logger:info_msg("~p: handle_client_cast(~p, ~p)\n", [?MODULE, Client, Msg]),
	send_msg(Msg, dict:fetch_keys(State)),
	{ok, State}.

handle_client_disconnect(Client, State) ->
	error_logger:info_msg("~p: handle_client_disconnect(~p)\n", [?MODULE, Client]),
	{ok, dict:erase(Client, State)}.

handle_app_cast(Msg, State) ->
	error_logger:info_msg("~p: handle_app_cast(~p)\n", [?MODULE, Msg]),
	send_msg(Msg, dict:fetch_keys(State)),
	{ok, State}.

handle_app_call(Msg, State) ->
	error_logger:info_msg("~p: handle_app_call(~p)\n", [?MODULE, Msg]),
	{reply, Msg, State}.

handle_terminate(State) ->
	error_logger:info_msg("~p: handle_terminate()\n", [?MODULE]),
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

send_msg(_Msg, []) -> ok;
send_msg(Msg, [Pid|T]) ->
	Pid ! Msg,
	send_msg(Msg, T).
