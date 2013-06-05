-module(kill_bill_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	KB_WA = {kb_webapp_sup, {kb_webapp_sup, start_link, []}, permanent, infinity, supervisor, [kb_webapp_sup]},
	KB_TEMPLATE = {kb_template_sup, {kb_template_sup, start_link, []}, permanent, infinity, supervisor, [kb_template_sup]},
	KB_APP = {kill_bill,{kill_bill, start_link, []}, permanent, 2000, worker, [kill_bill]},
	{ok, {{one_for_one, 5, 60}, [KB_APP, KB_TEMPLATE, KB_WA]}}.


