%%
%% Copyright 2013-14 Joaquim Rocha
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

-module(kb_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([remove_if_starts_with/2, 
		 remove_if_ends_with/2, 
		 implements_behaviour/2, 
		 to_hex/1,
		 upper/1]).

remove_if_starts_with(String, Search) ->
	case string:str(String, Search) of
		0 -> String;
		1 -> string:substr(String, 1 + string:len(Search));
		_ -> String
	end.

remove_if_ends_with(String, Search) ->
	Len = string:len(String) - string:len(Search) + 1,
	case string:rstr(String, Search) of
		0 -> String;
		Len -> string:substr(String, 1, Len - 1);
		_ -> String
	end.

implements_behaviour(Module, Behaviour) when is_atom(Module) andalso is_atom(Behaviour) ->
	Attr = Module:module_info(attributes),
	BiourList = proplists:get_value(behaviour, Attr, []),
	BiorList = proplists:get_value(behavior, Attr, []),
	FullList = BiourList ++ BiorList,
	lists:member(Behaviour, FullList);
implements_behaviour(_Module, _Behaviour) -> false.

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) -> 
	to_hex(binary_to_list(Bin));
to_hex([H|T]) -> 
	[to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

upper(Value) when is_binary(Value) ->
	List = binary_to_list(Value),
	Upper = upper(List),
	list_to_binary(Upper);
upper(Value) ->
	string:to_upper(Value).

%% ====================================================================
%% Internal functions
%% ====================================================================

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.
