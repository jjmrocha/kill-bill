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

-module(kb_dtl_util).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/4, execute/3]).

execute(Dtl, Args, #kb_request{context=Context, static=Static}) -> 
	Options = [{context, Context}, {static, Static}],
	execute_dtl(Dtl, Args, Options). 

execute(Dtl, none, Args, Req) -> execute(Dtl, Args, Req);
execute(Dtl, Dict, Args, #kb_request{context=Context, static=Static}) ->
	Options = [
			{resource, Dict},
			{context, Context},
			{static, Static}
			],
	execute_dtl(Dtl, Args, Options). 

%% ====================================================================
%% Internal functions
%% ====================================================================

execute_dtl(Dtl, Args, Options) -> 
	try Dtl:render(Args, Options) of
		{ok, IOList} -> {ok, IOList};
		{error, Err} -> {error, Err}
	catch
		Type:Error ->
			error_logger:error_msg("KB: Error executing DTL ~p: [~p:~p]\n", [Dtl, Type, Error]),
			{error, not_found}
	end. 
