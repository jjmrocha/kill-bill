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

-module(kb_json).

-define(JSX_CLEAN_OBJECT, [{}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([decode/1, encode/1, new/0, from_proplist/1]).

new() -> ?JSX_CLEAN_OBJECT.

from_proplist([]) -> ?JSX_CLEAN_OBJECT;
from_proplist(Props) -> Props.

encode(Value) ->
	jsx:encode(Value).

decode(Value) when is_binary(Value) ->
	case jsx:decode(Value) of
		?JSX_CLEAN_OBJECT -> [];
		Doc -> Doc
	end;		
decode(Value) when is_list(Value) ->
	decode(list_to_binary(Value)).

%% ====================================================================
%% Internal functions
%% ====================================================================


