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

-module(kill_bill_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	KB_RESOURCE = {kb_resource_sup, {kb_resource_sup, start_link, []}, permanent, infinity, supervisor, [kb_resource_sup]},
	KB_APP = {kill_bill,{kill_bill, start_link, []}, permanent, 2000, worker, [kill_bill]},
	{ok, {{one_for_one, 5, 60}, [KB_APP, KB_RESOURCE]}}.


