-module(kb_cowboy_action).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, [ResourceServer, ActionConfig]) ->
	{ok, Req, {ResourceServer, ActionConfig}}.

handle(Req, {ResourceServer, ActionConfig}) ->
	{Path, _} = cowboy_req:path(Req),
	Req2 = kb_template_util:execute(Path, ActionConfig, ResourceServer, Req),
	{ok, Req2, {ResourceServer, ActionConfig}}.

terminate(_Reason, _Req, _State) ->
	ok.