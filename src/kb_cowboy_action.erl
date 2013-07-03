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

init(_Transport, Req, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	ActionConfig = proplists:get_value(action_config, Opts),
	{ok, Req, {ResourceServer, ActionConfig#action_config.callback}}.

handle(Req, {ResourceServer, Handler}) ->
	{Method, Req1} = cowboy_req:method(Req),
	{Path, Req2} = cowboy_req:path_info(Req1),
	Req4 = case Method of
		<<"GET">> ->
				{QSVals, Req3} = cowboy_req:qs_vals(Req2),
				Response = Handler:get(Path, QSVals),
				handle_response(Response, ResourceServer, Req3);
		<<"POST">> ->
				{ok, BodyQS, Req3} = cowboy_req:body_qs(Req2),
				Response = Handler:post(Path, BodyQS),			
				handle_response(Response, ResourceServer, Req3);
		_ -> 
			Output = io_lib:format("Method [~p] not suported", [Method]),
			handle_response({raw, 501, [], Output}, ResourceServer, Req2)
	end,
	{ok, Req4, {ResourceServer, Handler}}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_response({html, Value}, ResourceServer, Req) -> 
	handle_response({raw, 200, [{<<"content-type">>, <<"text/html">>}], Value}, ResourceServer, Req);

handle_response({json, Value}, ResourceServer, Req) -> 
	Output = kb_json:encode(Value),
	handle_response({raw, 200, [{<<"content-type">>, <<"application/json">>}], Output}, ResourceServer, Req);

handle_response({dtl, Template, Args}, ResourceServer, Req) ->
	Dict = kb_http:get_dict(ResourceServer, Req),
	case kb_dtl_util:execute(Template, Dict, Args) of
		{ok, Html} ->
			handle_response({html, Html}, ResourceServer, Req);
		{error, not_found} ->
			Output = io_lib:format("Template [~p] not found", [Template]),
			handle_response({raw, 404, [], Output}, ResourceServer, Req);
		{error, Reason} ->
			Output = io_lib:format("Error ~p running template [~p]", [Reason, Template]),
			handle_response({raw, 500, [], Output}, ResourceServer, Req)
	end;

% Redirect code was copied from https://github.com/tsujigiri/axiom
handle_response({redirect, UrlOrPath}, _ResourceServer, Req) -> 
	{ok, UrlRegex} = re:compile("^(http|https)://"),
	{Url, Req1} = case re:run(UrlOrPath, UrlRegex) of
		{match, _} -> {UrlOrPath, Req};
		nomatch -> assemble_url(UrlOrPath, Req)
	end,
	{Method, Req2} = cowboy_req:method(Req1),
	{Version, Req3} = cowboy_req:version(Req2),
	Status = case {Version, Method} of
		{{1,1}, <<"GET">>} -> 302;
		{{1,1}, _} -> 303;
		_ -> 302
	end,
	Req4 = cowboy_req:set_meta(resp_status, Status, Req3),
	cowboy_req:set_resp_header(<<"Location">>, Url, Req4);
	
handle_response({raw, Status, Headers, Body}, _ResourceServer, Req) -> 
	{ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
	Req2.

assemble_url(Path, Req) ->
	{Host, Req1} = cowboy_req:host(Req),
	{Port, Req2} = cowboy_req:port(Req1),
	Url = [
		<<"http">>,
		case cowboy_req:get(transport, Req2) of
			ranch_ssl -> <<"s">>;
			_ -> <<>>
		end,
		<<"://">>,
		Host,
		case Port of
			80 -> <<>>;
			Port -> [<<":">>, integer_to_list(Port)]
		end,
		Path
	],
	{Url, Req2}.