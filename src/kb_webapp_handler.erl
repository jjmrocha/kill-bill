-module(kb_webapp_handler).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
	 	{handle_init, 0}, % {ok, State} | {stop, Reason :: term()}
	 	{handle_client_connect, 2},
        {handle_client_cast, 3},
		{handle_client_disconnect, 2},
		{handle_app_cast, 2},
		{handle_app_call, 2}, % {reply, Reply, NewState} | {stop, Reason, NewState}
		{handle_terminate, 1}
    ];
behaviour_info(_Other) ->
    undefined.
