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

-include("kill_bill.hrl").

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Data, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	Context = proplists:get_value(context, Opts),
	SessionManager = proplists:get_value(session_manager, Opts),
	Static = proplists:get_value(static, Opts),
	BaseRequest = #kb_request{context=list_to_binary(Context), 
							  resource_server=ResourceServer, 
							  session_manager=SessionManager, 
							  static=list_to_binary(Static)},
	{ok, Data, BaseRequest}.

handle(Data, BaseRequest) ->
	{Path, Data1} = cowboy_req:path_info(Data),
	NewPath = join(Path, <<>>),
	Request = BaseRequest#kb_request{data=Data1},
	Data2 = kb_template_util:execute(NewPath, Request),
	{ok, Data2, BaseRequest}.

terminate(_Reason, _Data, _State) ->
	ok.

join([], Value) -> Value;
join([H | T], <<>>) -> join(T, H);
join([H | T], Value) ->	join(T, <<Value/binary, $_, H/binary>>).