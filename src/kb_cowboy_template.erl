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

-module(kb_cowboy_template).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, Opts) ->
	ResourceServer = lists:keyfind(resource_server, 1, Opts),
	TemplateConfig = lists:keyfind(template_config, 1, Opts),
	{ok, Req, {ResourceServer, TemplateConfig}}.

handle(Req, {ResourceServer, TemplateConfig}) ->
	{Path, _} = cowboy_req:path(Req),
	io:format("~p: ~p~n", [?MODULE, Path]),
	Req2 = kb_template_util:execute(binary_to_list(Path), TemplateConfig, ResourceServer, Req),
	{ok, Req2, {ResourceServer, TemplateConfig}}.

terminate(_Reason, _Req, _State) ->
	ok.