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
	{ok, none}.

handle_client_connect(Client, State) -> 
	error_logger:info_msg("~p: handle_client_connect(~p, ~p)\n", [?MODULE, Client, State]),
	{ok, State}.

handle_client_cast(Client, Msg, State) ->
	error_logger:info_msg("~p: handle_client_cast(~p, ~p, ~p)\n", [?MODULE, Client, Msg, State]),
	{ok, State}.

handle_client_disconnect(Client, State) ->
	error_logger:info_msg("~p: handle_client_disconnect(~p, ~p)\n", [?MODULE, Client, State]),
	{ok, State}.

handle_app_cast(Msg, State) ->
	error_logger:info_msg("~p: handle_app_cast(~p, ~p)\n", [?MODULE, Msg, State]),
	{ok, State}.

handle_app_call(Msg, State) ->
	error_logger:info_msg("~p: handle_app_call(~p, ~p)\n", [?MODULE, Msg, State]),
	{reply, Msg, State}.

handle_terminate(State) ->
	error_logger:info_msg("~p: handle_terminate(~p)\n", [?MODULE, State]),
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================


