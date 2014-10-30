-module(examples_filter).

-behaviour(kb_filter_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(Method, Path, Req) ->
	Url = lists:foldl(fun(Part, Acc) -> 
			<<Acc/binary, $/, Part/binary>> 
		end, <<>>, Path),
	io:format("~s ~s/~n", [Method, Url]),
	{next, [], Req}.

%% ====================================================================
%% Internal functions
%% ====================================================================

	