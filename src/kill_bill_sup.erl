-module(kill_bill_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	KB_WA = {kb_webapp_sup, {kb_webapp_sup, start_link, []}, permanent, infinity, supervisor, [kb_webapp_sup]},
	KB_RESOURCE = {kb_resource_sup, {kb_resource_sup, start_link, []}, permanent, infinity, supervisor, [kb_resource_sup]},
	KB_APP = {kill_bill,{kill_bill, start_link, []}, permanent, 2000, worker, [kill_bill]},
	{ok, {{one_for_one, 5, 60}, [KB_APP, KB_RESOURCE, KB_WA]}}.


