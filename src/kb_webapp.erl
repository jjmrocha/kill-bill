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

-module(kb_webapp).

-behaviour(gen_server).

-define(ORIGIN_CLIENT, client).
-define(ORIGIN_APP, app).

-define(MSG_TYPE_CONNECT, connect).
-define(MSG_TYPE_INFO, info).
-define(MSG_TYPE_DISCONNECT, disconnect).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, app_call/2, app_cast/2, client_connect/1, client_disconnect/1, client_cast/2]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

app_call(WebApp, Msg) ->
	gen_server:call(WebApp, {?ORIGIN_APP, Msg}).

app_cast(WebApp, Msg) ->
	gen_server:cast(WebApp, {?ORIGIN_APP, Msg}).

client_connect(WebApp) ->
	gen_server:call(WebApp, {?ORIGIN_CLIENT, ?MSG_TYPE_CONNECT}).

client_disconnect(WebApp) ->
	gen_server:cast(WebApp, {?ORIGIN_CLIENT, self(), ?MSG_TYPE_DISCONNECT}).

client_cast(WebApp, Msg) ->
	gen_server:cast(WebApp, {?ORIGIN_CLIENT, self(), ?MSG_TYPE_INFO, Msg}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {callback, app_state}).

init([Callback]) ->
	case Callback:handle_init() of
		{ok, State} ->
			error_logger:info_msg("Starting WebApp callback ~p [~p]...\n", [Callback, self()]),
    		{ok, #state{callback=Callback, app_state=State}};
		{stop, Reason} ->
			error_logger:error_msg("Callback ~p not starting, because [~p]...\n", [Callback, Reason]),
			{stop, Reason}
	end.

handle_call({?ORIGIN_APP, Msg}, _From, State=#state{callback=Callback, app_state=Status}) ->
    case Callback:handle_app_call(Msg, Status) of
		{reply, Reply, NStatus} -> {reply, Reply, State#state{app_state=NStatus}};
		{stop, Reason, NStatus} -> {stop, Reason, State#state{app_state=NStatus}}
	end;
handle_call({?ORIGIN_CLIENT, ?MSG_TYPE_CONNECT}, From, State=#state{callback=Callback, app_state=Status}) ->
	case Callback:handle_client_connect(From, Status) of
		{ok, State} -> {reply, ok, State#state{app_state=State}};
		{refuse, Reason, State} ->
			error_logger:info_msg("Connection from ~p refused, because [~p]...\n", [From, Reason]),
			{reply, refuse, State#state{app_state=State}};
		{stop, Reason, State} ->
			error_logger:error_msg("Callback ~p stopping, because [~p]...\n", [Callback, Reason]),
			{stop, Reason, refuse, State#state{app_state=State}}
	end.

handle_cast({?ORIGIN_CLIENT, From, ?MSG_TYPE_DISCONNECT}, State=#state{callback=Callback, app_state=Status}) ->
	case Callback:handle_client_disconnect(From, Status) of
		{ok, State} -> {noreply, State#state{app_state=State}};
		{stop, Reason, State} -> 
			error_logger:error_msg("Callback ~p stopping, because [~p]...\n", [Callback, Reason]),
			{stop, Reason, State#state{app_state=State}}
	end;
handle_cast({?ORIGIN_CLIENT, From, ?MSG_TYPE_INFO, Msg}, State=#state{callback=Callback, app_state=Status}) ->
	case Callback:handle_client_cast(From, Msg, Status) of
		{ok, State} -> {noreply, State#state{app_state=State}};
		{stop, Reason, State} -> 
			error_logger:error_msg("Callback ~p stopping, because [~p]...\n", [Callback, Reason]),
			{stop, Reason, State#state{app_state=State}}
	end;
handle_cast({?ORIGIN_APP, Msg}, State=#state{callback=Callback, app_state=Status}) ->
	case Callback:handle_app_cast(Msg, Status) of
		{ok, State} -> {noreply, State#state{app_state=State}};
		{stop, Reason, State} -> 
			error_logger:error_msg("Callback ~p stopping, because [~p]...\n", [Callback, Reason]),
			{stop, Reason, State#state{app_state=State}}
	end.

handle_info(Info, State) ->
	error_logger:info_msg("handle_info(~p)\n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{callback=Callback, app_state=Status}) ->
	Callback:handle_terminate(Status),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


