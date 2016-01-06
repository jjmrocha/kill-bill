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

-define(SYSTEM_CHOSEN_LANGUAGE, chosen_language).

-define(ANY_LOCALE, any_locale).
-define(NO_COUNTRY_IN_LOCALE, none).

-record(kb_request, {
				context,
				static,
				action_prefix,
				resource_server,
				session_manager,
				session_key=none,
				session_data=none,
				session_saved=no,
				locales=none,
				resources=none,
				attributes=[],
				method,
				data
			}
		).