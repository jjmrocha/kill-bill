-module(kb_cowboy_toppage).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [TemplateServer]) ->
	{ok, Req, TemplateServer}.

handle(Req, TemplateServer) ->
	Locales = kb_http:get_accept_languages(Req),
	Html = gen_server:call(TemplateServer, {get_html, <<"/">>, Locales}),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],	Html, Req),
	{ok, Req2, TemplateServer}.

terminate(_Reason, _Req, _State) ->
	ok.