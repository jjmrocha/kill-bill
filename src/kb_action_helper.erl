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

-module(kb_action_helper).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	get_context/1,
	get_headers/1,
	get_args/1, 
	get_session/1,
	set_session/2,
	invalidate_session/1,
	set_cookie/4, 
	get_cookie/2,
	get_locales/1,
	set_locale/2
	]).

-spec get_context(Req :: #kb_request{}) -> binary().
get_context(Req) ->
	Req#kb_request.context.

-spec get_headers(Req :: #kb_request{}) -> {Headers, #kb_request{}}
	when Headers :: [{binary(), binary()}, ...].
get_headers(Req) ->
	{Headers, Req1} = cowboy_req:headers(Req#kb_request.data),
	{Headers, Req#kb_request{data=Req1}}.

-spec get_args(Req :: #kb_request{}) -> {Args, #kb_request{}}
	when Args :: [{binary(), binary()}, ...].
get_args(Req) ->
	case Req#kb_request.method of
		<<"GET">> ->
			{QSVals, Req1} = cowboy_req:qs_vals(Req#kb_request.data),
			{QSVals, Req#kb_request{data=Req1}};
		<<"POST">> ->
			{ok, BodyQS, Req1} = cowboy_req:body_qs(Req#kb_request.data),
			{BodyQS, Req#kb_request{data=Req1}};
		_ -> {[], Req}
	end.

-spec get_session(Req :: #kb_request{}) -> {SessionData, #kb_request{}}
	when SessionData :: [{any(), any()}, ...].
get_session(Req) ->
	kb_session_util:get_user_data(Req).

-spec set_session(SessionData, Req :: #kb_request{}) -> #kb_request{}
	when SessionData :: [{any(), any()}, ...].
set_session(UserData, Req) ->
	kb_session_util:set_user_data(UserData, Req).

-spec set_cookie(Name :: binary(), Value :: term(), MaxAge :: integer(), Req :: #kb_request{}) -> #kb_request{}.
set_cookie(Name, Value, MaxAge, Req) when is_binary(Value) ->
	Req1 = kb_http:set_cookie(Req#kb_request.context, Name, Value, MaxAge, Req#kb_request.data),
	Req#kb_request{data=Req1};
set_cookie(Name, Value, MaxAge, Req) ->
	set_cookie(Name, term_to_binary(Value), MaxAge, Req).

-spec invalidate_session(Req :: #kb_request{}) -> #kb_request{}.
invalidate_session(Req) ->
	case Req#kb_request.session_key of
		none -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1};
		SessionID -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, SessionID, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1}
	end.	

-spec get_cookie(Name :: binary(), Req :: #kb_request{}) -> {binary(), #kb_request{}}.
get_cookie(Name, Req) ->
	{Value, Req1} = kb_http:get_cookie(Name, Req#kb_request.data),
	{Value, Req#kb_request{data=Req1}}.

-spec get_locales(Req :: #kb_request{}) -> {any_locale | Locale, #kb_request{}}
	when Locale :: {Language :: binary(), Country :: binary()}.
get_locales(Req) ->
	kb_locale:get_locales(Req).

-spec set_locale(Locale, Req :: #kb_request{}) -> #kb_request{}
	when Locale :: {Language :: binary(), Country :: binary()}.
set_locale(Locale, Req) ->
	kb_locale:set_locale(Locale, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================
