-module(kb_template_util).

-include("kill_bill.hlr").

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/4]).

execute(Path, TemplateConfig, ResourceServer, Req) -> 
	Dtl = get_dtl(Path, TemplateConfig),
	Dict = get_dict(ResourceServer, Req),
	{ok, Req2} =  case kb_dtl_util:execute(Dtl, Dict, []) of
		{ok, Html} ->
			cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],	Html, Req);
		{error, not_found} ->
			Output = io_lib:format("Template [~p] not found", [Path]),
			cowboy_req:reply(404, [], Output, Req);
		{error, Reason} ->
			Output = io_lib:format("Error ~p running template [~p]", [Reason, Path]),
			cowboy_req:reply(500, [], Output, Req)
	end,
	Req2.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_dtl(Path, TemplateConfig) -> 
	NoExt = kb_util:remove_if_ends_with(Path, TemplateConfig#template.extension),
	NoPath = case string:rstr(NoExt, "/") of
		0 -> NoExt;
		Pos -> string:substr(NoExt, Pos + 1)
	end,
	list_to_atom(NoPath ++ "_dtl").

get_dict(none, _Req) -> none;
get_dict(ResourceServer, Req) ->
	Locales = kb_http:get_accept_languages(Req),
	kb_resource:get_resource(ResourceServer, Locales).
