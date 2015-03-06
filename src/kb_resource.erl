%%
%% Copyright 2013 Joaquim Rocha
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(kb_resource).

-include("kill_bill.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, stop/1, get_resource/2, add_locale/3]).

start_link(Config) ->
	gen_server:start_link(?MODULE, [Config], []).

stop(Server) ->
	gen_server:cast(Server, {stop}).

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
	{noreply, Store};
handle_cast({stop}, State) ->
	{stop, normal, State}.

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

load_resources(Config, Store) -> 
	BaseName = proplists:get_value(base_name, Config, "message"),
	FileExtension = proplists:get_value(file_extension, Config, ".txt"),
	FileDirDefault = "./resource",
	FileDir = case lists:keyfind(priv_dir, 1, Config) of
		{_, App, Path} ->
			case code:priv_dir(App) of
				{error, bad_name} ->
					error_logger:error_msg("~p: Can't resolve the priv_dir of application ~p\n", [?MODULE, App]),
					FileDirDefault;
				PrivDir -> PrivDir ++ "/" ++ Path
			end;
		false -> proplists:get_value(dir, Config, FileDirDefault)
	end,
	Extension = "." ++ kb_util:remove_if_starts_with(FileExtension, "."),
	Wildcard = BaseName ++ "*" ++ Extension,
	Files = filelib:wildcard(Wildcard, FileDir),
	add_resources(FileDir, BaseName, Extension, Files, Store).

add_resources(_Dir, _Basename, _Extension, [], Store) -> Store;
add_resources(Dir, Basename, Extension, [Filename|Tail], Store) ->
	FullName = filename:join(Dir, Filename),
	case file:script(FullName) of
		{ok, ValueList} ->
			Locale = get_locale_from_filename(Basename, Extension, Filename),
			NStore = dict:store(Locale, convert_to_dict(ValueList), Store),
			add_resources(Dir, Basename, Extension, Tail, NStore);
		{error, {Line, _Mod, _Term}} ->
			error_logger:error_msg("KB: Error in line [~p] of file ~s\n", [Line, Filename]),
			add_resources(Dir, Basename, Extension, Tail, Store);
		{error, Reason} ->
			error_logger:error_msg("KB: Error [~p] reading file ~s\n", [Reason, Filename]),
			add_resources(Dir, Basename, Extension, Tail, Store)		
	end.

convert_to_dict(ValueList) ->
	Dict = dict:new(),
	load_dict(ValueList, Dict).

load_dict([], Dict) -> Dict;
load_dict([{Key, Value}|Tail], Dict) ->
	load_dict(Tail, dict:store(Key, Value, Dict)).

get_locale_from_filename(Basename, Extension, Filename) ->
	NoBasename = string:substr(Filename, string:len(Basename) + 1),
	NoExtension = string:substr(NoBasename, 1, string:len(NoBasename) - string:len(Extension)),
	case NoExtension of
		[] -> ?ANY_LOCALE;
		_ ->
			CleanLocation = kb_util:remove_if_starts_with(NoExtension, "_"),
			case string:str(CleanLocation, "_") of
				0 -> {list_to_binary(CleanLocation), ?NO_COUNTRY_IN_LOCALE};
				Pos -> 
					L = string:substr(CleanLocation, 1, Pos - 1),
					C = string:substr(CleanLocation, Pos + 1),
					{list_to_binary(L), list_to_binary(string:to_upper(C))}
			end
	end.

run(Locales, From, Store) ->
	Server = self(),
	Fun = fun() ->
			Reply = case dict:size(Store) of
				0 -> dict:new();
				1 -> find(Server, ?ANY_LOCALE, Store);
				_ -> find(Server, Locales, Store)
			end,
			gen_server:reply(From, Reply)
	end,
	spawn(Fun).

find(_Server, ?ANY_LOCALE, Store) -> 
	case dict:find(?ANY_LOCALE, Store) of
		{ok, Value} -> Value;
		error -> dict:new()
	end;
find(Server, [], Store) -> find(Server, ?ANY_LOCALE, Store);
find(Server, [Locale|Tail], Store) -> 
	case dict:find(Locale, Store) of
		{ok, Value} -> Value;
		error ->
			{L, _} = Locale,
			case dict:find({L, ?NO_COUNTRY_IN_LOCALE}, Store) of
				{ok, Value} ->
					kb_resource:add_locale(Server, Locale, Value),
					Value;
				error ->
					find(Server, Tail, Store)
			end
	end.

