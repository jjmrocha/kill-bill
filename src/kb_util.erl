-module(kb_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([remove_if_starts_with/2, remove_if_ends_with/2]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================


