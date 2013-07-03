-module(hello_world_action).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get/2, post/2]).

get([], _Args) ->
	{html, "<html><body>Fast as a speeding bullet</body></html>"};

get([<<"json">>, Id, <<"create">>], Args) ->
	Name = proplists:get_value(<<"name">>, Args, <<"Joaquim">>),
	Surname = proplists:get_value(<<"surname">>, Args, <<"Rocha">>),
	Obj = {struct, [{id, Id}, {name, Name}, {surname, Surname}]},
	{json, Obj};

get([<<"args">>], Args) ->
	{dtl, args_list_dtl, [{arg_list, Args}]};

get([<<"form">>], _Args) ->
	{dtl, form_dtl, []};

get([<<"google">>], _Args) ->
	{redirect, <<"http://www.google.com">>};

get(_Path, _Args) ->
	{raw, 404, [], "Not found!"}.


post([<<"form">>], Args) ->
	{dtl, form_dtl, [{arg_list, Args}]};

post(_Path, _Args) ->
	{raw, 404, [], "Not found!"}.

%% ====================================================================
%% Internal functions
%% ====================================================================


