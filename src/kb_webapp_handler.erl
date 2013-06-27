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

-module(kb_webapp_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
	 	{handle_init, 0}, % {ok, State} | {stop, Reason :: term()}
	 	{handle_client_connect, 2}, % {ok, State} | {refuse, Reason :: term(), State} | {stop, Reason, State}
        {handle_client_cast, 3}, % {ok, State} | {stop, Reason, State}
		{handle_client_disconnect, 2}, % {ok, State} | {stop, Reason, State}
		{handle_app_cast, 2}, % {ok, State} | {stop, Reason, State}
		{handle_app_call, 2}, % {reply, Reply, State} | {stop, Reason, State}
		{handle_terminate, 1} % {ok, State}
    ];
behaviour_info(_Other) ->
    undefined.
