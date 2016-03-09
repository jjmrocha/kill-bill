%%
%% Copyright 2016 Joaquim Rocha <jrocha@gmailbox.org>
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

-define(KB_APP_DEPLOYED_EVENT, <<"kill_bill:app_deployed">>).
-define(KB_APP_UNDEPLOYED_EVENT, <<"kill_bill:add_undeployed">>).
-define(KB_SERVER_STARTED_EVENT, <<"kill_bill:server_started">>).
-define(KB_SERVER_STOPPED_EVENT, <<"kill_bill:server_stopped">>).

-define(KB_EVENT_SERVER_PROP, server).

-define(KB_APP_EVENT_INFO(Server), #{?KB_EVENT_SERVER_PROP => Server}).
