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
	WebApp = proplists:get_value(web_app, Opts),
	case kb_webapp:client_connect(WebApp) of
		ok -> {ok, Req, WebApp};
		refuse -> {shutdown, Req} 
	end.

stream(Msg, Req, WebApp) ->
	kb_webapp:client_cast(WebApp, Msg),
	{ok, Req, WebApp}.

info(Msg, Req, WebApp) ->
	{reply, Msg, Req, WebApp}.

terminate(_Req, WebApp) ->
	kb_webapp:client_disconnect(WebApp),
	ok.