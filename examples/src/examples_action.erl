-module(examples_action).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(_Method, [], Req) ->
	{html, "<html><body>Fast as a speeding bullet</body></html>", Req};

handle(_Method, [<<"json">>, Id, <<"create">>], Req) ->
	{Args, Req1} = kb_helper:get_args(Req),
	Name = proplists:get_value(<<"name">>, Args, <<"Joaquim">>),
	Surname = proplists:get_value(<<"surname">>, Args, <<"Rocha">>),
	Obj = {struct, [{id, Id}, {name, Name}, {surname, Surname}]},
	{json, Obj, Req1};

handle(_Method, [<<"args">>], Req) ->
	{Args, Req1} = kb_helper:get_args(Req),
	{dtl, args_list_dtl, [{arg_list, Args}], Req1};

handle(<<"GET">>, [<<"form">>], Req) ->
	{dtl, form_dtl, [], Req};
handle(<<"POST">>, [<<"form">>], Req) ->
	{Args, Req1} = kb_helper:get_args(Req),
	{dtl, form_dtl, [{arg_list, Args}], Req1};

handle(_Method, [<<"google">>], Req) ->
	{redirect, <<"http://www.google.com">>, Req};

handle(_Method, [<<"locale">>, Language], Req) ->
	Req1 = kb_helper:set_locale({binary_to_list(Language), none}, Req),
	Output = io_lib:format("<html><body><a href=\"~s\">back</a></body></html>", [kb_helper:get_context(Req1)]),
	{html, Output, Req1};

handle(_Method, [<<"client">>], Req) ->
	{Args, Req1} = kb_helper:get_args(Req),
	Msg = proplists:get_value(<<"msg">>, Args, <<"Hello 2">>),
	kill_bill:cast_webclient(examples, client, Msg), 
	{html, "<html><body>Done!</body></html>", Req1};

handle(_Method, _Path, Req) ->
	{raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================


