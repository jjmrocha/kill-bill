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

-module(kb_session_util).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_user_data/1,
	set_user_data/2,
	get_system_data/1,
	set_system_property/3,
	remove_system_property/2]).

get_user_data(Req) ->
	{SessionData, Req1} = get_session_data(Req),
	{_, UserData} = lists:keyfind(user, 1, SessionData),
	{UserData, Req1}.

set_user_data(UserData, Req) ->
	{SessionData, Req1} = get_session_data(Req),
	NSessionData = lists:keystore(user, 1, SessionData, {user, UserData}),
	store_session_data(Req1#kb_request{session_data=NSessionData}).

get_system_data(Req) ->
	{SessionData, Req1} = get_session_data(Req),
	{_, SystemData} = lists:keyfind(system, 1, SessionData),
	{SystemData, Req1}.

set_system_property(Key, Value, Req) ->
	{SystemData, Req1} = get_system_data(Req),
	NSystemData = lists:keystore(Key, 1, SystemData, {Key, Value}),
	SessionData = lists:keystore(system, 1, Req1#kb_request.session_data, {system, NSystemData}),
	store_session_data(Req1#kb_request{session_data=SessionData}).

remove_system_property(Key, Req) ->
	{SystemData, Req1} = get_system_data(Req),
	NSystemData = lists:keydelete(Key, 1, SystemData),
	SessionData = lists:keystore(system, 1, Req1#kb_request.session_data, {system, NSystemData}),
	store_session_data(Req1#kb_request{session_data=SessionData}).	

%% ====================================================================
%% Internal functions
%% ====================================================================

get_session_data(Req) when is_record(Req, kb_request) ->
	case Req#kb_request.session_data of 
		none -> 
			case kb_session:get_session(Req#kb_request.session_manager, Req#kb_request.data) of
				{no_session, SessionData, Data1} ->
					{SessionData, Req#kb_request{data=Data1, session_data=SessionData}};
				{SessionID, SessionData, Data1} ->
					{SessionData, Req#kb_request{data=Data1, session_key=SessionID, session_data=SessionData}}
			end;
		SessionData ->
			{SessionData, Req}
	end.

store_session_data(Req) when is_record(Req, kb_request) ->
	case Req#kb_request.session_key of
		none -> 
			{SessionID, Data1} = kb_session:set_session(Req#kb_request.session_manager, Req#kb_request.session_data, Req#kb_request.data),
			Req#kb_request{session_saved=yes, session_key=SessionID, data=Data1};
		SessionID -> 
			{_SessionID, Data1} = kb_session:set_session(Req#kb_request.session_manager, SessionID, Req#kb_request.session_data, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1}
	end.
