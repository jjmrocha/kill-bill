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

-module(kb_locale).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_locales/1,
	set_locale/2,
	remove_locale/1]).

get_locales(Req=#kb_request{locales=none}) -> 
	{Locales, Req1} = find_locales(Req),
	{Locales, Req1#kb_request{locales=Locales}};
get_locales(Req=#kb_request{locales=Locales}) -> {Locales, Req}.

set_locale(Locale, Req) ->
	Req1 = kb_session_util:set_system_property(?SYSTEM_CHOSEN_LANGUAGE, Locale, Req),
	Req1#kb_request{locales=Locale, resources=none}.

remove_locale(Req) -> 
	Req1 = kb_session_util:remove_system_property(?SYSTEM_CHOSEN_LANGUAGE, Req),
	{Locales, Data1} = get_accept_languages(Req1#kb_request.data),
	Req1#kb_request{locales=Locales, resources=none, data=Data1}.

%% ====================================================================
%% Internal functions
%% ====================================================================

find_locales(Req) ->
	case get_user_locale(Req) of
		{none, Req1} ->
			{Locales, Data1} = get_accept_languages(Req1#kb_request.data),
			{Locales, Req1#kb_request{data=Data1}};
		{Locale, Req1} -> {[Locale], Req1}
	end.

get_user_locale(Req) -> 
	{SystemData, Req1} = kb_session_util:get_system_data(Req),
	ChosenLanguage = case lists:keyfind(?SYSTEM_CHOSEN_LANGUAGE, 1, SystemData) of
		false -> none;
		{_, Lang} -> Lang
	end,
	{ChosenLanguage, Req1}.

get_accept_languages(Data) ->
	{AcceptLanguages, Data1} = kb_http:get_header(<<"accept-language">>, Data),
	case AcceptLanguages of
		undefined -> {[], Data1};
		error -> {[], Data1};
		_ ->
			Fun = fun({_TagA, QualityA}, {_TagB, QualityB}) -> 
					QualityA > QualityB 
			end,
			SortedAcceptLanguages = lists:sort(Fun, AcceptLanguages),
			{get_locales(SortedAcceptLanguages, []), Data1}
	end.

get_locales([], []) -> ?ANY_LOCALE;
get_locales([], List) -> lists:reverse(List);
get_locales([H|T], List) ->
	{AcceptLanguage, _Quality} = H,
	case get_locale(AcceptLanguage) of
		?ANY_LOCALE -> 
			case List of
				[] -> ?ANY_LOCALE;
				_ -> get_locales(T, List)
			end;
		Locale -> get_locales(T, [Locale|List])
	end.

get_locale('*') -> ?ANY_LOCALE;
get_locale(AcceptLanguage) ->
	case binary:split(AcceptLanguage, [<<"-">>]) of
		[Language] -> {Language, ?NO_COUNTRY_IN_LOCALE};
		[Language, Country] -> {Language, kb_util:upper(Country)}
	end.
