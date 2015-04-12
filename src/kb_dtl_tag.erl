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

-module(kb_dtl_tag).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	message/2,
	context/2,
	static/2]).

message(Args, Options) ->
	case lists:keyfind(key, 1, Args) of
		false -> <<"{error -> use: message key=\"key_value\"}">>;
		{_, Key} ->
			case lists:keyfind(resource, 1, Options) of
				false -> no_value(Key);
				{_, Dict} ->
					case dict:find(Key, Dict) of
						error -> no_value(Key);
						{ok, Text} ->
							MsgArgs = get_message_args(Args, Dict, []),
							kb_resource_util:replace(Text, MsgArgs)
					end
			end
	end.

context(Args, Options) ->
	Context = case lists:keyfind(context, 1, Options) of
		false ->
			error_logger:error_msg("KB: No context\n"),
			<<"/">>;
		{_, Other} -> Other
	end,
	context_out(Context, Args).

static(Args, Options) ->
	Static = case lists:keyfind(static, 1, Options) of
		false ->
			error_logger:error_msg("KB: No static info\n"),
			<<"/">>;
		{_, S} -> S
	end,	
	File = case lists:keyfind(file, 1, Args) of
		false ->
			error_logger:error_msg("KB: No file in static tag\n"),
			<<"">>;
		{_, F} -> F
	end,
	context([{file, join(Static, File)}], Options).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_message_args([{key, _}|T], Dict, Out) -> get_message_args(T, Dict, Out);
get_message_args([{Arg, Value}|T], Dict, Out) ->
	BinArg = atom_to_binary(Arg, utf8),
	NewValue = transform_value(Value, Dict),
	get_message_args(T, Dict, [{BinArg, NewValue}|Out]);
get_message_args([], _Dict, Out) -> Out.

transform_value(Value = <<"key=", Key/binary>>, Dict) ->
	case dict:find(Key, Dict) of
		error -> Value;
		{ok, NewValue} -> NewValue
	end;
transform_value(Value, _Dict) -> Value.

no_value(Key) -> <<"{", Key/binary, "}">>.

context_out(Context, []) -> Context;
context_out(Context, [{file, Url}|_]) -> join(Context, Url);
context_out(Context, [_|T]) -> context_out(Context, T).

join(Context, <<$/, Url/binary>>) -> join(Context, Url);
join(Context, Url) -> <<Context/binary, Url/binary>>.
