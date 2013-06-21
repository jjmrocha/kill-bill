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

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/3, execute/2]).

execute(Dtl, Args) -> 
	try render(Dtl, Args) of
		Result -> Result
	catch
		_Type:_Error -> {error, not_found}
	end. 

execute(Dtl, none, Args) -> execute(Dtl, Args);
execute(Dtl, Dict, Args) ->
	Fun = fun(Val) ->
		case dict:find(Val, Dict) of
			error -> "{" ++ Val ++ "}";
			{ok, Text} -> Text
		end
	end,
	Options = [{translation_fun, Fun}],
	
	try render(Dtl, Args, Options) of
		Result -> Result
	catch
		_Type:_Error -> {error, not_found}
	end. 

%% ====================================================================
%% Internal functions
%% ====================================================================

render(Dtl, Args) ->
	case Dtl:render(Args) of
		{ok, IOList} -> {ok, IOList};
		{error, Err} -> {error, Err}
	end.

render(Dtl, Args, Options) ->
	case Dtl:render(Args, Options) of
		{ok, IOList} -> {ok, IOList};
		{error, Err} -> {error, Err}
	end.
