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

-module(kb_template_util).

-include("kill_bill.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/2]).

execute(Path, Req) -> 
	Dtl = get_dtl(Path),
	kb_response:handle({dtl, Dtl, [], Req}).

%% ====================================================================
%% Internal functions
%% ====================================================================


get_dtl(Path) when is_binary(Path) -> 
	get_dtl(binary_to_list(Path));
get_dtl(Path) -> 
	list_to_atom(Path ++ "_dtl").
