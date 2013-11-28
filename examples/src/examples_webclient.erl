-module(examples_webclient).

-behaviour(kb_webclient_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_init/1,
	handle_client_connect/3,
	handle_client_cast/3,
	handle_client_disconnect/2,
	handle_app_cast/2,
	handle_app_call/2,
	handle_terminate/1]).

handle_init(_Context) -> 
	error_logger:info_msg("~p: handle_init()\n", [?MODULE]),
	{ok, []}.

handle_client_connect(Client, no_session, State) -> 
	error_logger:info_msg("~p: handle_client_connect(~p, ~p)\n", [?MODULE, Client, no_session]),
	{refuse, "No session, can't connect",State};
handle_client_connect(Client, Session, State) -> 
	error_logger:info_msg("~p: handle_client_connect(~p, ~p)\n", [?MODULE, Client, Session]),
	{ok, [Client|State]}.

handle_client_cast(Client, Msg, State) ->
	error_logger:info_msg("~p: handle_client_cast(~p, ~p)\n", [?MODULE, Client, Msg]),
	send_msg(Msg, remove(Client, State)),
	{ok, State}.

handle_client_disconnect(Client, State) ->
	error_logger:info_msg("~p: handle_client_disconnect(~p)\n", [?MODULE, Client]),
	{ok, remove(Client, State)}.

handle_app_cast(Msg, State) ->
	error_logger:info_msg("~p: handle_app_cast(~p)\n", [?MODULE, Msg]),
	send_msg(Msg, State),
	{ok, State}.

handle_app_call(Msg, State) ->
	error_logger:info_msg("~p: handle_app_call(~p)\n", [?MODULE, Msg]),
	{reply, Msg, State}.

handle_terminate(_State) ->
	error_logger:info_msg("~p: handle_terminate()\n", [?MODULE]),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

send_msg(_Msg, []) -> ok;
send_msg(Msg, [Pid|T]) ->
	Pid ! Msg,
	send_msg(Msg, T).

remove(Client, ClientList) ->
	lists:delete(Client, ClientList).
