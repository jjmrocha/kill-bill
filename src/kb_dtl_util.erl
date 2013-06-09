-module(kb_dtl_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/3, execute/2]).

execute(Dtl, Args) -> 
	try render(Dtl, Args) of
		Result -> Result
	catch
		_Type:_Error -> {error, not_found}
	end. 

execute(Dtl, none, Args) -> execute(Dtl, Args);
execute(Dtl, Dict, Args) ->
	Fun = fun(Val) ->
		case dict:find(Val, Dict) of
			error -> "{" ++ Val ++ "}";
			{ok, Text} -> Text
		end
	end,
	Options = [{translation_fun, Fun}],
	
	try render(Dtl, Args, Options) of
		Result -> Result
	catch
		_Type:_Error -> {error, not_found}
	end. 

%% ====================================================================
%% Internal functions
%% ====================================================================

render(Dtl, Args) ->
	case Dtl:render(Args) of
		{ok, IOList} -> {ok, IOList};
		{error, Err} -> {error, Err}
	end.

render(Dtl, Args, Options) ->
	case Dtl:render(Args, Options) of
		{ok, IOList} -> {ok, IOList};
		{error, Err} -> {error, Err}
	end.
