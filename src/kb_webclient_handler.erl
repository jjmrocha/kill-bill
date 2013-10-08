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

-module(kb_webclient_handler).

-include("kill_bill.hrl").

-callback handle_init(WebClientContext :: #kb_webclient_context{}) 
	-> {ok, State :: term()}.

-callback handle_client_connect(Client :: pid(), SessionID :: binary(), State :: term()) 
	-> {ok, State :: term()} 
	| {refuse, Reason :: term(), State :: term()}.

-callback handle_client_cast(Client :: pid(), Msg :: term(), State :: term()) 
	-> {ok, State :: term()}.

-callback handle_client_disconnect(Client :: pid(), State :: term()) 
	-> {ok, State :: term()}.

-callback handle_app_cast(Msg :: term(), State :: term()) 
	-> {ok, State :: term()}.

-callback handle_app_call(Msg :: term(), State :: term()) 
	-> {ok, State :: term()}.

-callback handle_terminate(State :: term()) -> ok.
