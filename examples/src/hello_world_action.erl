-module(hello_world_action).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get/2, post/2]).

get([], _Args) ->
	{html, "<html><body>Fast as a speeding bullet</body></html>"};

get([<<"json">>], _Args) ->
	Obj = "Isto é json",
	{json, Obj};

get([<<"json">>, <<"object">>], _Args) ->
	Obj = {obj, [{id, 1}, {name, "Joaquim"}, {surname, "Rocha"}]},
	{json, Obj}.

post(_Path, _Args) ->
	{raw, 404, [], "Not found!"}.

%% ====================================================================
%% Internal functions
%% ====================================================================


