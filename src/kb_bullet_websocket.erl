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

-module(kb_bullet_websocket).

-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, Opts, _Active) ->
	Webclient = proplists:get_value(webclient_app, Opts),
	SessionManager = proplists:get_value(session_manager, Opts),
	case kb_webclient:client_connect(Webclient) of
		ok -> {ok, Req, {Webclient, SessionManager}};
		refuse -> {shutdown, Req} 
	end.

stream(<<"kb-ping">>, Req, {Webclient, SessionManager}) ->
	{ok, Req2} = kb_session:touch_session(SessionManager, Req),
	{reply, <<"kb-pong">>, Req2, {Webclient, SessionManager}};
stream(Msg, Req, {Webclient, SessionManager}) ->
	kb_webclient:client_cast(Webclient, Msg),
	{ok, Req2} = kb_session:touch_session(SessionManager, Req),
	{ok, Req2, {Webclient, SessionManager}}.

info(Msg, Req, {Webclient, SessionManager}) ->
	{reply, Msg, Req, {Webclient, SessionManager}}.

terminate(_Req, {Webclient, _SessionManager}) ->
	kb_webclient:client_disconnect(Webclient),
	ok.