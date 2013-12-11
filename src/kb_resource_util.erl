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

-module(kb_resource_util).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_dict/1,
		 get_message/2,
		 get_message/3,
		 replace/2]).

get_dict(Req=#kb_request{resource_server=none}) -> {none, Req};
get_dict(Req=#kb_request{resource_server=ResourceServer, resources=none}) ->
	{Locales, Req1} = kb_locale:get_locales(Req),
	Dict = kb_resource:get_resource(ResourceServer, Locales),
	{Dict, Req1#kb_request{resources=Dict}};
get_dict(Req=#kb_request{resources=Dict}) -> {Dict, Req}.

get_message(MsgId, Req) ->
	get_message(MsgId, [], Req).

get_message(MsgId, Args, Req) ->
	case get_dict(Req) of
		{none, Req1} -> {no_resource, Req1};
		{Dict, Req1} ->
			case dict:find(MsgId, Dict) of
				error -> {message_not_found, Req1};
				{ok, Msg} ->
					NewMsg = replace(Msg, Args),
					{NewMsg, Req1}
			end
	end.

replace(Message, [{Key, Value}|T]) ->
	SKey = get_search_key(Key),
	NewMsg = re:replace(Message, SKey, Value, [global, {return, binary}]),
	replace(NewMsg, T);
replace(Message, []) -> Message.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_search_key(Key = <<"{", _/binary>>) ->	Key;
get_search_key(Key) when is_list(Key) -> 
	get_search_key(list_to_binary(Key));
get_search_key(Key) -> <<"{", Key/binary, "}">>.
