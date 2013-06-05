-module(kb_template).

-behaviour(gen_server).

-include("kill_bill.hlr").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {config, templates, messages, final}).

init([Config]) ->
    {ok, #state{config=Config, templates=ets:new(templates), messages=ets:new(messages), final=ets:new(final)}}.

handle_call({get_html, Path, Locales}, From, State) ->
	run(Path, Locales, From, State),
    {noreply, State}.

handle_cast(Msg, State) ->
	error_logger:info_msg("handle_cast(~p)\n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
	error_logger:info_msg("handle_info(~p)\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

run(<<"/">>, Locales, From, State=#state{config=Config}) ->
	TopPage = list_to_binary(Config#template.top_page),
	run(TopPage, Locales, From, State);
run(Path, Locales, From, State) ->
	Fun = fun() ->
		Reply = find(Path, Locales, State),
		gen_server:reply(From, Reply)
	end,
	spawn(Fun).

find(_Path, _Locales, _State) -> todo.
				  
