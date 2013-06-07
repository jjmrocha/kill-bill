-module(kb_cowboy_template).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [ResourceServer, TemplateConfig]) ->
	{ok, Req, {ResourceServer, TemplateConfig}}.

handle(Req, {ResourceServer, TemplateConfig}) ->
	{Path, _} = cowboy_req:path(Req),
	Req2 = kb_template_util:execute(Path, TemplateConfig, ResourceServer, Req),
	{ok, Req2, {ResourceServer, TemplateConfig}}.

terminate(_Reason, _Req, _State) ->
	ok.