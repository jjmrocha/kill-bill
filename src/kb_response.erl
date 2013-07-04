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

-module(kb_response).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/4]).

handle({html, Value}, Context, ResourceServer, Req) -> 
	handle({raw, 200, [{<<"content-type">>, <<"text/html">>}], Value}, Context, ResourceServer, Req);

handle({json, Value}, Context, ResourceServer, Req) -> 
	Output = kb_json:encode(Value),
	handle({raw, 200, [{<<"content-type">>, <<"application/json">>}], Output}, Context, ResourceServer, Req);

handle({dtl, Template, Args}, Context, ResourceServer, Req) ->
	Dict = kb_http:get_dict(ResourceServer, Req),
	case kb_dtl_util:execute(Context, Template, Dict, Args) of
		{ok, Html} ->
			handle({html, Html}, Context, ResourceServer, Req);
		{error, not_found} ->
			Output = io_lib:format("Template [~p] not found", [Template]),
			handle({raw, 404, [], Output}, Context, ResourceServer, Req);
		{error, Reason} ->
			Output = io_lib:format("Error ~p running template [~p]", [Reason, Template]),
			handle({raw, 500, [], Output}, Context, ResourceServer, Req)
	end;

handle({redirect, Url}, Context, ResourceServer, Req) when is_list(Url) -> 
	handle({redirect, list_to_binary(Url)}, Context, ResourceServer, Req);
handle({redirect, Url}, Context, ResourceServer, Req) when is_binary(Url) -> 
	handle({raw, 302, [{<<"Location">>, Url}], []}, Context, ResourceServer, Req);
	
handle({raw, Status, Headers, Body}, _Context, _ResourceServer, Req) -> 
	{ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
	Req2.

%% ====================================================================
%% Internal functions
%% ====================================================================


