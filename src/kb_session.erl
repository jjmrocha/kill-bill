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
	narciso:uuid().

-spec get_session_id(Data :: cowboy_req:req()) -> 
	{no_session, cowboy_req:req()} | {binary(), cowboy_req:req()}.
get_session_id(Data) ->
	case kb_http:get_cookie(?SESSION_COOKIE, Data) of
		{undefined, Data1} -> {no_session, Data1};
		{SessionID, Data1} -> {SessionID, Data1}
	end.

-spec get_session(CacheName :: atom(), Data :: cowboy_req:req()) ->
	{no_session, list(), cowboy_req:req()} | {binary(), list(), cowboy_req:req()}.
get_session(none, Data) -> {no_session, ?SESSION_DATA, Data};
get_session(CacheName, Data) ->
	case get_session_id(Data) of
		{no_session, Data1} -> {no_session, ?SESSION_DATA, Data1};
		{SessionID, Data1} -> 
			case g_cache:get(CacheName, SessionID) of
				{ok, SessionData, _Version} -> {SessionID, SessionData, Data1};
				_ -> {SessionID, ?SESSION_DATA, Data1}
			end
	end.

-spec set_session(CacheName :: atom(), SessionData :: list(), Data :: cowboy_req:req()) ->
	{binary(), cowboy_req:req()}.
set_session(none, _SessionData, Data) -> {none, Data};
set_session(CacheName, SessionData, Data) ->
	SessionID = create_session_id(),
	Data1 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, none, Data),
	set_session(CacheName, SessionID, SessionData, Data1).

-spec set_session(CacheName :: atom(), SessionID :: binary(), SessionData :: list(), Data :: cowboy_req:req()) ->
	{binary(), cowboy_req:req()}.
set_session(none, _SessionID, _SessionData, Data) -> {none, Data};
set_session(CacheName, SessionID, SessionData, Data) ->
	g_cache:store(CacheName, SessionID, SessionData),
	{SessionID, Data}.

-spec invalidate_session(CacheName :: atom(), Data :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
invalidate_session(none, Data) -> {ok, Data};
invalidate_session(CacheName, Data) ->
	case get_session_id(Data) of
		{no_session, Data1} -> {ok, Data1};
		{SessionID, Data1} -> invalidate_session(CacheName, SessionID, Data1)
	end.

-spec invalidate_session(CacheName :: atom(), SessionID :: binary(), Data :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
invalidate_session(none, _SessionID, Data) -> {ok, Data};
invalidate_session(CacheName, SessionID, Data) ->
	Data1 = kb_http:set_cookie(?SESSION_PATH, ?SESSION_COOKIE, SessionID, 0, Data),
	g_cache:remove(CacheName, SessionID),
	{ok, Data1}.

-spec touch_session(CacheName :: atom(), Data :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
touch_session(none, Data) -> {ok, Data};
touch_session(CacheName, Data) ->
	case get_session_id(Data) of
		{no_session, Data1} -> {ok, Data1};
		{SessionID, Data1} -> touch_session(CacheName, SessionID, Data1)
	end.

-spec touch_session(CacheName :: atom(), SessionID :: binary(), Data :: cowboy_req:req()) ->
	{ok, cowboy_req:req()}.
touch_session(none, _SessionID, Data) -> {ok, Data};
touch_session(CacheName, SessionID, Data) ->
	g_cache:touch(CacheName, SessionID),
	{ok, Data}.

%% ====================================================================
%% Internal functions
%% ====================================================================
