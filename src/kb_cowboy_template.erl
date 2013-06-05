-module(kb_cowboy_template).

-include("kill_bill.hlr").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [TemplateServer]) ->
	{ok, Req, TemplateServer}.

handle(Req, TemplateServer) ->
	{Path, _} = cowboy_req:path(Req),
	Locales = kb_http:get_accept_languages(Req),
	case gen_server:call(TemplateServer, {get_html, Path, Locales}) of
		{ok, Html} ->
			{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],	Html, Req),
			{ok, Req2, TemplateServer};
		{error, not_found} ->
			Output = io_lib:format("Template [~p] not found", [Path]),
			{ok, Req2} = cowboy_req:reply(404, [], Output, Req),
			{ok, Req2, TemplateServer};
		{error, Reason} ->
			Output = io_lib:format("Error ~p running template [~p]", [Reason, Path]),
			{ok, Req2} = cowboy_req:reply(500, [], Output, Req),
			{ok, Req2, TemplateServer}
	end.

terminate(_Reason, _Req, _State) ->
	ok.