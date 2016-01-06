-module(examples_filter).

-behaviour(kb_filter_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(Method, Path, Req) ->
	Context = kb_action_helper:get_context(Req),
	ActionPrefix = kb_action_helper:get_action_prefix(Req),
	Url = lists:foldl(fun(Part, Acc) -> 
			<<Acc/binary, $/, Part/binary>> 
		end, <<>>, Path),
	io:format("~s ~s~s~s/~n", [Method, Context, ActionPrefix, Url]),
	{next, [], Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

	