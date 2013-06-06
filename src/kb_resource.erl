-module(kb_resource).

-behaviour(gen_server).

-include("kill_bill.hlr").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, get_resource/2, add_locale/3]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

get_resource(Server, Locales) ->
	gen_server:call(Server, {resource, Locales}).

add_locale(Server, Locale, Resource) ->
	gen_server:cast(Server, {add_locale, Locale, Resource}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([Config]) ->
    {ok, load_resources(Config, dict:new())}.

handle_call({resource, Locales}, From, State) ->
	run(Locales, From, State),
    {noreply, State}.

handle_cast({add_locale, Locale, Resource}, State) ->
	Store = dict:store(Locale, Resource, State),
    {noreply, Store}.

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

load_resources(_Config, _Store) -> todo.

run(Locales, From, Store) ->
	Server = self(),
	Fun = fun() ->
		Reply = find(Server, Locales, Store),
		gen_server:reply(From, Reply)
	end,
	spawn(Fun).

find(_Server, _Locales, _Store) -> todo.
				  
