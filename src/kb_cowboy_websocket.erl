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

-module(kb_cowboy_websocket).

-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({_TransportName, _ProtocolName}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, [WebApp]) ->
	case kb_webapp:client_connect(WebApp) of
		ok -> {ok, Req, WebApp};
		refuse -> {shutdown, Req} 
	end.

websocket_handle({text, Msg}, Req, WebApp) ->
	kb_webapp:client_cast(WebApp, Msg),
	{ok, Req, WebApp};
websocket_handle(_Data, Req, WebApp) ->
	{ok, Req, WebApp}.

websocket_info(Msg, Req, WebApp) ->
	{reply, {text, Msg}, Req, WebApp}.

websocket_terminate(_Reason, _Req, WebApp) ->
	kb_webapp:client_disconnect(WebApp),
	ok.