-module(kb_resource_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_resource_server/1]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

start_resource_server(TemplateConfig) ->
	supervisor:start_child(?MODULE, [TemplateConfig]).

init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),

    ResourceServer = {kb_template, {kb_resource, start_link, []}, temporary, 2000, worker, [kb_resource]},
    {ok,{{simple_one_for_one, 10, 60}, [ResourceServer]}}.