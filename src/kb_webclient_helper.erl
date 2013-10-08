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

-module(kb_webclient_helper).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	get_session/2,
	set_session/3
	]).

get_session(SessionID, Ctx) when is_binary(SessionID) andalso is_record(Ctx, kb_webclient_context) ->
	case get_session_data(Ctx#kb_webclient_context.session_manager, SessionID) of
		session_expired -> session_expired;
		SessionData -> 
			{_, UserData} = lists:keyfind(user, 1, SessionData),
			UserData
	end.

set_session(SessionID, UserData, Ctx) when is_binary(SessionID) andalso is_list(UserData) andalso is_record(Ctx, kb_webclient_context) ->
	case get_session_data(Ctx#kb_webclient_context.session_manager, SessionID) of
		session_expired -> session_expired;
		SessionData -> 
			NSessionData = lists:keystore(user, 1, SessionData, {user, UserData}),
			g_cache:store(Ctx#kb_webclient_context.session_manager, SessionID, NSessionData),
			ok
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_session_data(CacheName, SessionID) ->
	case g_cache:get(CacheName, SessionID) of
		{ok, SessionData} -> SessionData;
		_ -> session_expired
	end.
