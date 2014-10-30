%%
%% Copyright 2013-14 Joaquim Rocha
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
	get_session_id/1,
	get_session/2, 
	set_session/3, 
	set_session/4,
	invalidate_session/2, 
	invalidate_session/3, 
	touch_session/2,
	touch_session/3]).

-spec create_session_id() -> binary().
create_session_id() ->
	%% Code provided by Andrey Sergienko 
	%% http://www.asergienko.com/erlang-how-to-create-uuid-or-session-id/
	Now = {_, _, Micro} = erlang:now(),
	Nowish = calendar:now_to_universal_time(Now),
	Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
	Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
	list_to_binary(Prefix ++ kb_util:to_hex(crypto:rand_bytes(9))).

-spec get_session_id(Req :: cowboy_req:req()) -> 
	{no_session, cowboy_req:req()} | {binary(), cowboy_req:req()}.
get_session_id(Req) ->
	case kb_http:get_cookie(?SESSION_COOKIE, Req) of
		{undefined, Req2} -> {no_session, Req2};
		{SessionID, Req2} -> {SessionID, Req2}
	end.

-spec get_session(CacheName :: atom(), Req :: cowboy_req:req()) ->
	{no_session, list(), cowboy_req:req()} | {binary(), list(), cowboy_req:req()}.
get_session(none, Req) -> {no_session, ?SESSION_DATA, Req};
get_session(CacheName, Req) ->
	case get_session_id(Req) of
		{no_session, Req2} -> {no_session, ?SESSION_DATA, Req2};
		{SessionID, Req2} -> 
			case g_cache:get(CacheName, SessionID) of
				{ok, SessionData} -> {SessionID, SessionData, Req2};
				_ -> {SessionID, ?SESSION_DATA, Req2}
			end
	end.

-spec set_session(CacheName :: atom(), SessionData :: list(), Req :: cowboy_req:req()) ->
	{binary(), cowboy_req:req()}.
set_session(none, _SessionData, Req) -> {none, Req};
set_session(CacheName, SessionData, Req) ->
	SessionID = create_session_id(),
	Req2 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, none, Req),
	set_session(CacheName, SessionID, SessionData, Req2).

-spec set_session(CacheName :: atom(), SessionID :: binary(), SessionData :: list(), Req :: cowboy_req:req()) ->
	{binary(), cowboy_req:req()}.
set_session(none, _SessionID, _SessionData, Req) -> {none, Req};
set_session(CacheName, SessionID, SessionData, Req) ->
	g_cache:store(CacheName, SessionID, SessionData),
	{SessionID, Req}.

-spec invalidate_session(CacheName :: atom(), Req :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
invalidate_session(none, Req) -> {ok, Req};
invalidate_session(CacheName, Req) ->
	case get_session_id(Req) of
		{no_session, Req2} -> {ok, Req2};
		{SessionID, Req2} -> invalidate_session(CacheName, SessionID, Req2)
	end.

-spec invalidate_session(CacheName :: atom(), SessionID :: binary(), Req :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
invalidate_session(none, _SessionID, Req) -> {ok, Req};
invalidate_session(CacheName, SessionID, Req) ->
	Req2 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, 0, Req),
	g_cache:remove(CacheName, SessionID),
	{ok, Req2}.

-spec touch_session(CacheName :: atom(), Req :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
touch_session(none, Req) -> {ok, Req};
touch_session(CacheName, Req) ->
	case get_session_id(Req) of
		{no_session, Req2} -> {ok, Req2};
		{SessionID, Req2} -> touch_session(CacheName, SessionID, Req2)
	end.

-spec touch_session(CacheName :: atom(), SessionID :: binary(), Req :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
touch_session(none, _SessionID, Req) -> {ok, Req};
touch_session(CacheName, SessionID, Req) ->
	g_cache:touch(CacheName, SessionID),
	{ok, Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================
