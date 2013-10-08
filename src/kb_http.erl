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

-module(kb_http).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_header/2, get_cookie/2, set_cookie/5]).

get_header(Name, Req) ->
	{ok, Header, Req1} = cowboy_req:parse_header(Name, Req),
	{Header, Req1}.

set_cookie(Path, CookieName, Value, MaxAge, Req) ->
	Opts = case MaxAge of
		none -> [{path, Path}];
		_ -> [{path, Path}, {max_age, MaxAge}] 
	end,
	cowboy_req:set_resp_cookie(CookieName, Value, Opts, Req).

get_cookie(CookieName, Req) ->
	case cowboy_req:cookie(CookieName, Req) of
		{undefined, Req2} -> {undefined, Req2};
		{Value, Req2} -> {Value, Req2}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

