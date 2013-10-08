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

-module(kb_webclient_helper).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	get_session/2,
	set_session/3
	]).

-spec get_session(SessionID :: binary(), Ctx :: #kb_webclient_context{}) -> session_expired | SessionData
	when SessionData :: [{any(), any()}, ...].
get_session(SessionID, Ctx) ->
	kb_session_util:get_user_data(SessionID, Ctx).

-spec set_session(SessionID :: binary(), SessionData, Ctx :: #kb_webclient_context{}) -> session_expired | ok
	when SessionData :: [{any(), any()}, ...].
set_session(SessionID, UserData, Ctx) ->
	kb_session_util:set_user_data(SessionID, UserData, Ctx).

%% ====================================================================
%% Internal functions
%% ====================================================================

