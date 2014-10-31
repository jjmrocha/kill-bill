%%
%% Copyright 2013-14 Joaquim Rocha
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

-module(kb_action_handler).

-include("kill_bill.hrl").

-callback handle(Method :: binary(), Path :: list(), Request :: #kb_request{}) 
	-> {html, Value :: iodata(), Request :: #kb_request{}} 
	| {json, Value :: any(), Request :: #kb_request{}}
    | {json, Status :: integer(), Headers :: list(), Value :: any(), Request :: #kb_request{}}
	| {dtl, Template :: module(), Args :: list(), Request :: #kb_request{}}
	| {redirect, Url :: iolist(), Request :: #kb_request{}} 
	| {raw, Status :: integer(), Headers :: list(), Body :: iodata(), Request :: #kb_request{}}.