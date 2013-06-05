-module(kb_webapp_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_webapp/1]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

start_webapp(Callback) ->
	supervisor:start_child(?MODULE, [Callback]).

init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),

    WebApp = {kb_webapp, {kb_webapp, start_link, []}, temporary, 2000, worker, [kb_webapp]},
    {ok,{{simple_one_for_one, 10, 60}, [WebApp]}}.