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

-module(kb_cowboy_action_full).

-behaviour(cowboy_http_handler).

-include("kill_bill.hlr").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	ActionConfig = proplists:get_value(action_config, Opts),
	Context = proplists:get_value(context, Opts),
	{ok, Req, {ResourceServer, ActionConfig#action_config.callback, list_to_binary(Context)}}.

handle(Req, {ResourceServer, Handler, Context}) ->
	{Method, Req1} = cowboy_req:method(Req),
	{Path, Req2} = cowboy_req:path_info(Req1),
	Request = #kb_request{context=Context, method=Method, data=Req2},
	{Response, Request1} = Handler:handle(Method, Path, Request),
	Req3 = kb_response:handle(Response, Context, ResourceServer, Request1#kb_request.data),
	{ok, Req3, {ResourceServer, Handler, Context}}.

terminate(_Reason, _Req, _State) ->
	ok.