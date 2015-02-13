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

-module(kb_cowboy_action).

-behaviour(cowboy_http_handler).

-include("kill_bill.hrl").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Data, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	CallbackList = proplists:get_value(callback_list, Opts),
	Context = proplists:get_value(context, Opts),
	SessionManager = proplists:get_value(session_manager, Opts),
	{ok, Data, {ResourceServer, SessionManager, CallbackList, list_to_binary(Context)}}.

handle(Data, {ResourceServer, SessionManager, CallbackList, Context}) ->
	{Method, Data1} = cowboy_req:method(Data),
	{Path, Data2} = cowboy_req:path_info(Data1),
	Request = #kb_request{context=Context, 
			resource_server=ResourceServer, 
			session_manager=SessionManager, 
			method=Method, 
			data=Data2},
	Response = handle(CallbackList, Path, Request),
	Data3 = kb_response:handle(Response),
	{ok, Data3, {ResourceServer, SessionManager, CallbackList, Context}}.

terminate(_Reason, _Data, _State) ->
	ok.

handle([Callback|T], Path, Request = #kb_request{method=Method}) ->
	case Callback:handle(Method, Path, Request) of
		{next, Attributes, Request1} ->
			handle(T, Path, Request1#kb_request{attributes=Attributes});
		Response -> Response
	end;
handle([], _Path, Request) -> {raw, 500, [], <<"No handler for request">>, Request}.