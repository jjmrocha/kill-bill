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

-module(kb_helper).

-include("kill_bill.hlr").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	get_context/1,
	get_args/1, 
	get_session/1,
	set_session/2,
	invalidate_session/1,
	set_cookie/4, 
	get_cookie/2,
	get_locale/1,
	set_locale/2
	]).

get_context(Req) when is_record(Req, kb_request) ->
	Req#kb_request.context.

get_args(Req) when is_record(Req, kb_request) ->
	case Req#kb_request.method of
		<<"GET">> ->
			{QSVals, Req1} = cowboy_req:qs_vals(Req#kb_request.data),
			{QSVals, Req#kb_request{data=Req1}};
		<<"POST">> ->
			{ok, BodyQS, Req1} = cowboy_req:body_qs(Req#kb_request.data),
			{BodyQS, Req#kb_request{data=Req1}};
		_ -> {[], Req}
	end.

get_session(Req) when is_record(Req, kb_request) ->
	{SessionData, Req1} = get_session_data(Req),
	UserData = proplists:get_value(user, SessionData),
	{UserData, Req1}.

set_session(UserData, Req) when is_list(UserData) andalso is_record(Req, kb_request) ->
	{SessionData, Req1} = get_session_data(Req),
	NSessionData = lists:keystore(user, 1, SessionData, {user, UserData}),
	store_session_data(Req1#kb_request{session_data=NSessionData}).

set_cookie(Name, Value, MaxAge, Req) when is_binary(Name) andalso is_binary(Value) andalso is_integer(MaxAge) andalso is_record(Req, kb_request) ->
	Req1 = kb_http:set_cookie(Req#kb_request.context, Name, Value, MaxAge, Req#kb_request.data),
	Req#kb_request{data=Req1};
set_cookie(Name, Value, MaxAge, Req) when is_binary(Name) andalso is_integer(MaxAge) andalso is_record(Req, kb_request) ->
	set_cookie(Name, term_to_binary(Value), MaxAge, Req).

invalidate_session(Req) when is_record(Req, kb_request) ->
	case Req#kb_request.session_key of
		none -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1};
		SessionID -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, SessionID, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1}
	end.	

get_cookie(Name, Req) when is_binary(Name) andalso is_record(Req, kb_request) ->
	{Value, Req1} = kb_http:get_cookie(Name, Req#kb_request.data),
	{Value, Req#kb_request{data=Req1}}.

get_locale(Req) when is_record(Req, kb_request) ->
	{SystemData, Req1} = get_system_data(Req),
	ChosenLanguage = proplists:get_value(?SYSTEM_CHOSEN_LANGUAGE, SystemData, none),
	{ChosenLanguage, Req1}.

set_locale(Locale, Req) when is_tuple(Locale) andalso is_record(Req, kb_request) ->
	set_system_property(?SYSTEM_CHOSEN_LANGUAGE, Locale, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

set_system_property(Key, Value, Req) when is_atom(Key) andalso is_record(Req, kb_request) ->
	{SystemData, Req1} = get_system_data(Req),
	NSystemData = lists:keystore(Key, 1, SystemData, {Key, Value}),
	SessionData = lists:keystore(system, 1, Req1#kb_request.session_data, {system, NSystemData}),
	store_session_data(Req1#kb_request{session_data=SessionData}).

get_system_data(Req) when is_record(Req, kb_request) ->
	{SessionData, Req1} = get_session_data(Req),
	SystemData = proplists:get_value(system, SessionData),
	{SystemData, Req1}.

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