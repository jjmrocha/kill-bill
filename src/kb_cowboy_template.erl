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
	{_, ResourceServer} = lists:keyfind(resource_server, 1, Opts),
	{ok, Req, {ResourceServer}}.

handle(Req, {ResourceServer}) ->
	{Path, Req1} = cowboy_req:path_info(Req),
	NewPath = join(Path, <<>>),
	Req2 = kb_template_util:execute(NewPath, ResourceServer, Req1),
	{ok, Req2, {ResourceServer}}.

terminate(_Reason, _Req, _State) ->
	ok.

join([], Value) -> Value;
join([H | T], <<>>) -> join(T, H);
join([H | T], Value) ->	join(T, <<Value/binary, $_, H/binary>>).