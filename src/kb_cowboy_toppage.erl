-module(kb_cowboy_toppage).

-include("kill_bill.hlr").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [ResourceServer, TemplateConfig]) ->
	{ok, Req, {ResourceServer, TemplateConfig}}.

handle(Req, {ResourceServer, TemplateConfig}) ->
	Req2 = kb_template_util:execute(TemplateConfig#template.top_page, TemplateConfig, ResourceServer, Req),
	{ok, Req2, {ResourceServer, TemplateConfig}}.

terminate(_Reason, _Req, _State) ->
	ok.