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

-module(kb_http).

-define(COOKIE_CHOSEN_LANGUAGE, <<"kb-chosen-language">>).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_accept_languages/1, set_chosen_language/3, set_cookie/4, get_dict/2]).

get_accept_languages(Req) ->
	{ChosenLanguage, Req2} = cowboy_req:cookie(?COOKIE_CHOSEN_LANGUAGE, Req),
	case ChosenLanguage of
		undefined ->
			{ok, AcceptLanguages, _Req} = cowboy_req:parse_header(<<"accept-language">>, Req2),
			case AcceptLanguages of
				undefined -> [];
				AcceptLanguages ->
					Fun = fun({_TagA, QualityA}, {_TagB, QualityB}) -> QualityA > QualityB end,
					AcceptLanguage2 = lists:sort(Fun, AcceptLanguages),
					get_locales(AcceptLanguage2, [])
			end;
		_ -> [get_locale(binary_to_list(ChosenLanguage))]
	end.

set_chosen_language(Path, Locale, Req) when is_tuple(Locale) ->
	BinLocale = case Locale of
		{Language, none} -> list_to_binary(Language);
		{Language, Country} -> list_to_binary(Language ++ "-" ++ Country)
	end,
	set_cookie(Path, ?COOKIE_CHOSEN_LANGUAGE, BinLocale, Req).

get_dict(none, _Req) -> none;
get_dict(ResourceServer, Req) ->
	Locales = get_accept_languages(Req),
	kb_resource:get_resource(ResourceServer, Locales).

set_cookie(Path, CookieName, Value, Req) ->
	cowboy_req:set_resp_cookie(CookieName, Value, [{path, Path}], Req).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_locales([], List) -> lists:reverse(List);
get_locales([H|T], List) ->
	{Lang, _Quality} = H,
	Language = binary_to_list(Lang),
	case get_locale(Language) of
		none -> get_locales(T, List);
		Locale -> get_locales(T, [Locale|List])
	end.

get_locale('*') -> none;
get_locale(Language) ->
	case string:str(Language, "-") of
		0 -> {Language, none};
		Pos -> 
			L = string:substr(Language, 1, Pos - 1),
			C = string:substr(Language, Pos + 1),
			{L, string:to_upper(C)}
	end.
