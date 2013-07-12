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
-export([get_args/1, 
	set_cookie/4, 
	get_cookie/2,
	set_locale/2,
	get_context/1]).

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

set_cookie(Name, Value, MaxAge, Req) when is_binary(Name) andalso is_binary(Value) andalso is_integer(MaxAge) andalso is_record(Req, kb_request) ->
	Req1 = kb_http:set_cookie(Req#kb_request.context, Name, Value, MaxAge, Req#kb_request.data),
	Req#kb_request{data=Req1}.

get_cookie(Name, Req) when is_binary(Name) andalso is_record(Req, kb_request) ->
	{Value, Req1} = cowboy_req:cookie(Name, Req#kb_request.data),
	{Value, Req#kb_request{data=Req1}}.

set_locale(Locale, Req) when is_tuple(Locale) andalso is_record(Req, kb_request) ->
	Req1 = kb_http:set_chosen_language(Req#kb_request.context, Locale, Req#kb_request.data),
	Req#kb_request{data=Req1}.

get_context(Req) when is_record(Req, kb_request) ->
	Req#kb_request.context.

%% ====================================================================
%% Internal functions
%% ====================================================================


