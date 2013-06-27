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

-module(kb_cowboy_action).

-behaviour(cowboy_http_handler).

-include("kill_bill.hlr").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [ResourceServer, ActionConfig]) ->
	{ok, Req, {ResourceServer, ActionConfig}}.

handle(Req, {ResourceServer, ActionConfig}) ->
	Handler = ActionConfig#action_config.callback,
	{Method, Req1} = cowboy_req:method(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	NoExtPath = kb_util:remove_if_ends_with(Path, ActionConfig#action_config.extension),
	SplitPath = case binary:split(NoExtPath, <<"/">>, [global, trim]) of
		[<<>> | Rest] -> Rest;
		Else -> Else
	end,
	Req4 = case Method of
		<<"GET">> ->
				{QSVals, Req3} = cowboy_req:qs_vals(Req2),
				Response = Handler:get(SplitPath, QSVals),
				handle_response(Response, Req3);
		<<"POST">> ->
				{ok, BodyQS, Req3} = cowboy_req:body_qs(Req2),
				Response = Handler:post(SplitPath, BodyQS),			
				handle_response(Response, Req3);
		_ -> 
			Output = io_lib:format("Action [~p] not found", [Path]),
			{ok, Req3} = cowboy_req:reply(404, [], Output, Req2),
			Req3	
	end,
	{ok, Req4, {ResourceServer, ActionConfig}}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_response({html, _Value}, _Req) -> todo;
handle_response({json, _Value}, _Req) -> todo;
handle_response({dtl, _Template, _Args}, _Req) -> todo;
handle_response({redirect, _Url}, _Req) -> todo;
handle_response({raw, _Status, _Headers, _Body}, _Req) -> todo.