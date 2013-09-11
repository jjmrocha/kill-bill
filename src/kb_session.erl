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

-module(kb_session).

-define(SESSION_COOKIE, <<"kb_session">>).
-define(SESSION_PATH, <<"/">>).
-define(SESSION_DATA, [{system, []}, {user, []}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_session_id/0, 
	get_session_id/2,
	get_session/2, 
	set_session/3, 
	set_session/4,
	invalidate_session/2, 
	invalidate_session/3, 
	touch_session/2,
	touch_session/3]).

create_session_id() ->
	%% Code provided by Andrey Sergienko 
	%% http://www.asergienko.com/erlang-how-to-create-uuid-or-session-id/
	Now = {_, _, Micro} = erlang:now(),
	Nowish = calendar:now_to_universal_time(Now),
	Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
	Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
	list_to_binary(Prefix ++ kb_util:to_hex(crypto:rand_bytes(9))).

get_session_id(CacheName, Req) when is_atom(CacheName) ->
	case kb_http:get_cookie(?SESSION_COOKIE, Req) of
		{undefined, Req2} -> {no_session, Req2};
		{SessionID, Req2} -> {SessionID, Req2}
	end.

get_session(CacheName, Req) when is_atom(CacheName) ->
	case kb_http:get_cookie(?SESSION_COOKIE, Req) of
		{undefined, Req2} -> {no_session, ?SESSION_DATA, Req2};
		{SessionID, Req2} -> 
			case g_cache:get(CacheName, SessionID) of
				{ok, SessionData} -> {SessionID, SessionData, Req2};
				_ -> {SessionID, ?SESSION_DATA, Req2}
			end
	end.

set_session(CacheName, SessionData, Req) when is_atom(CacheName) andalso is_list(SessionData) ->
	SessionID = create_session_id(),
	Req2 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, none, Req),
	set_session(CacheName, SessionID, SessionData, Req2).

set_session(CacheName, SessionID, SessionData, Req) when is_atom(CacheName) andalso is_binary(SessionID) andalso is_list(SessionData) ->
	g_cache:store(CacheName, SessionID, SessionData),
	{SessionID, Req}.

invalidate_session(CacheName, Req) when is_atom(CacheName) ->
	case kb_http:get_cookie(?SESSION_COOKIE, Req) of
		{undefined, Req2} -> {ok, Req2};
		{SessionID, Req2} -> invalidate_session(acheName, SessionID, Req2)
	end.

invalidate_session(CacheName, SessionID, Req) when is_atom(CacheName) andalso is_binary(SessionID) ->
	Req2 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, 0, Req),
	g_cache:remove(CacheName, SessionID),
	{ok, Req2}.

touch_session(CacheName, Req) when is_atom(CacheName)  ->
	case kb_http:get_cookie(?SESSION_COOKIE, Req) of
		{undefined, Req2} -> {ok, Req2};
		{SessionID, Req2} -> touch_session(CacheName, SessionID, Req2)
	end.

touch_session(CacheName, SessionID, Req) when is_atom(CacheName) andalso is_binary(SessionID) ->
	g_cache:touch(CacheName, SessionID),
	{ok, Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================
