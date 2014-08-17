-module(examples_action).

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(_Method, [], Req) ->
	{html, "<html><body>Fast as a speeding bullet</body></html>", Req};

handle(_Method, [<<"json">>, Id, <<"create">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	Name = proplists:get_value(<<"name">>, Args, <<"Joaquim">>),
	Surname = proplists:get_value(<<"surname">>, Args, <<"Rocha">>),
	Obj = [{id, Id}, {name, Name}, {surname, Surname}],
	{json, Obj, Req1};

handle(_Method, [<<"args">>], Req) ->
	{Args, Req1} = kb_action_helper:get_args(Req),
	{dtl, args_list_dtl, [{arg_list, Args}], Req1};

handle(<<"GET">>, [<<"form">>], Req) ->
	{Session, Req1} = kb_action_helper:get_session(Req),
	List = proplists:get_value(list, Session, []),
	{dtl, form_dtl, [{list, List}], Req1};
handle(<<"POST">>, [<<"form">>], Req) ->
	%% Retrive data from post
	{Args, Req1} = kb_action_helper:get_args(Req),
	Id = proplists:get_value(<<"id">>, Args, none),
	Name = proplists:get_value(<<"name">>, Args, none),
	%% Retrive data from session
	{Session, Req2} = kb_action_helper:get_session(Req1),
	List = proplists:get_value(list, Session, []),
	%% Add new tuple da list
	NewList = [{Id, Name}|List],
	%% Update session data
	NewSession = lists:keystore(list, 1, Session, {list, NewList}),
	Req3 = kb_action_helper:set_session(NewSession, Req2),
	%% Generate response
	{dtl, form_dtl, [{list, NewList}], Req3};

handle(_Method, [<<"google">>], Req) ->
	{redirect, <<"http://www.google.com">>, Req};

handle(_Method, [<<"locale">>, Language], Req) ->
	Req1 = kb_action_helper:set_locale({Language, none}, Req),
	Output = io_lib:format("<html><body><a href=\"~s\">back</a></body></html>", [kb_action_helper:get_context(Req1)]),
	{html, Output, Req1};

handle(_Method, [<<"invalidate">>], Req) ->
	Req1 = kb_action_helper:invalidate_session(Req),
	Output = io_lib:format("<html><body><a href=\"~s\">back</a></body></html>", [kb_action_helper:get_context(Req1)]),
	{html, Output, Req1};

handle(_Method, _Path, Req) ->
	{raw, 404, [], "Not found!", Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================


