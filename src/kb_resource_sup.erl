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

-module(kb_resource_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_resource_server/1]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

start_resource_server(ResourceConfig) ->
	supervisor:start_child(?MODULE, [ResourceConfig]).

init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),
	
	ResourceServer = {kb_resource, {kb_resource, start_link, []}, temporary, 2000, worker, [kb_resource]},
	{ok,{{simple_one_for_one, 10, 60}, [ResourceServer]}}.