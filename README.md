Kill-Bill
=========
*Web Application Server*


Installation
------------

Using rebar:

```erlang
{deps, [
	{kill_bill, ".*", {hg, "https://bitbucket.org/jjmrocha/kill-bill", "1.0.0"}}
]}.
```


Start Kill-Bill
---------------

Kill-Bill depends on cowboy, jsondoc, erlydtl and gibreel, you need to start cowboy and gibreel before kill-bill.

```erlang
%% Start cowboy
ok = application:start(crypto),
ok = application:start(ranch),
ok = application:start(cowlib),
ok = application:start(cowboy),
%% Start gibreel
ok = application:start(columbo),
ok = application:start(cclock),
ok = application:start(gibreel),
%% Start Kill-Bill
ok = application:start(kill_bill).
```


Server Management
-----------------

### API

```erlang
%% Setup a server
kill_bill:config_server(ServerConfig :: Config) -> {ok, ServerName :: atom()} | {error, Reason :: any()} 
	when Config :: ConfigFile :: string()
                     | {server_config, ServerName :: atom(), Config :: list()}.

%% Start server
kill_bill:start_server(ServerName :: atom()) -> ok | {error, Reason :: any()}.

%% Stop server
kill_bill:stop_server(ServerName :: atom()) -> ok | {error, Reason :: any()}.

%% List all servers
kill_bill:get_server_list() -> list().
```

### Server configuration file

```erlang
{server_config, ServerName, [
	{host, Host}, 
	{protocol, Protocol}, 
	{ssl, SSLConfig},
	{port, Port},
	{acceptor_number, AcceptorNumber},
	{max_connections, MaxConnections}
]}.
```

Where:

* ServerName - **Server name (it's an atom and is mandatory)**
* Host - **Host (defaults to '_' - all hosts), examples: "127.0.0.1", "www.gmailbox.org" or "[www.]gmailbox.org"**
* Protocol - **Http protocol (http or https - defaults to http)**
* SSLConfig - **Use with protocol = https (defaults to none), it's a proplist with the following properties: cacertfile, certfile and keyfile**
* Port - **Port number (defaults to 8080)**
* AcceptorNumber - **Number of processes accepting connections (defaults to 100)**
* MaxConnections - **Max number of connections (defaults to infinity)**

Example:
Accept connections on port 8080 for all hosts.
```erlang
{server_config, my_server, []}.
```


Web Application Management
--------------------------

### API

```erlang
%% Deploy a WebApp
kill_bill:deploy(ServerName :: atom(), WebAppConfig :: Config) -> ok | {error, Reason :: any()} 
	when Config :: WebAppFile :: string()
                     | {webapp_config, WebAppName :: atom(), Config :: list()}.	

%% Undeploy WebApp
kill_bill:undeploy(WebAppName :: atom()) -> ok | {error, Reason :: any()}.

%% List all WebApps
kill_bill:get_webapp_list() -> WebAppList
	when WebAppList :: [{WebAppName :: atom(), ServerName :: atom()}, ...].
```

### WebApp configuration file

```erlang
{webapp_config, WebAppName, [
	{context, Context},
	{template, TemplateConfig},
	{resource, ResourceConfig},
	{action, ActionList},
	{static, StaticConfig},
	{session_timeout, SesionTimeout}
]}.
```

Where:

* WebAppName - **WebApp name (it's an atom and is mandatory)**
* Context - **WebApp context (defaults to "/")**
* TemplateConfig - **Erlydtl template access configuration (defaults to none), it's a proplist with the following properties: top_page and prefix**
	* top_page - **Default page (equivalent to index.html, defaults to "index")**
	* prefix - **URL to template, i.e. /Context/Prefix/TemplateName**
* ResourceConfig - **Message files (defaults to none), it's a proplist with the following properties: base_name, file_extension and file_dir**
	* base_name - **Name of the message file (defaults to "message"), the full format is: file_dir/base_name[_language[_country]]file_extension**
	* file_extension - **Extension of the message files (defaults to ".txt")**
	* file_dir - **Directory where the message files are located (defaults to "./resource")**
* ActionList - **List of actions (prefix and callback pairs), each action is define as: {ActionPrefix, ActionCallback}, where:**
	* ActionPrefix - **URL of action, i.e. /Context/ActionPrefix**
	* ActionCallback - **Name of the erlang module, must implement the ```kb_action_handler``` callback behaviour**
* StaticConfig - **Static content configuration (defaults to none), it's a proplist with the following properties: path and dir**
	* path - **URL to file (defaults to "/"), i.e. /Context/Path/FileName**
	* dir - **Directory with static content (defaults to "./static")**
* SesionTimeout - **Session timeout in minutes (defaults to 30)**

### Example

The following code was removed from the example project, that you will find on the "examples" directory.
```erlang
{ok, ServerID} = kill_bill:config_server("./priv/default-server.config"),
ok = kill_bill:deploy(ServerID, "./priv/root-webapp.config"),
ok = kill_bill:deploy(ServerID, "./priv/examples-webapp.config"),
ok = kill_bill:start_server(ServerID).
```


kb_action_handler callback
--------------------------

The actions must implement the ```kb_action_handler``` callback behaviour.

```erlang
handle(Method :: binary(), Path :: list(), Request :: #kb_request{}) 
	-> {html, Value :: iodata(), Request1 :: #kb_request{}} 
	 | {json, Value :: any(), Request1 :: #kb_request{}}
	 | {dtl, Template :: module(), Args :: list(), Request1 :: #kb_request{}}
	 | {redirect, Url :: iolist(), Request1 :: #kb_request{}} 
	 | {raw, Status :: integer(), Headers :: list(), Body :: iodata(), Request1 :: #kb_request{}}.
```

Parameters:

* Method - **HTTP Method (GET, POST, ...)**
* Path - **The URL split by "/", i.e. the request ```GET /shop/hardware``` will produce the path = [<<"shop">>, <<"hardware">>]**
* Request - **Record with the request data**

### Example

WebApp configuration file:
```erlang
{webapp_config, examples, [
	{context, "/examples/"},
	{action, [ 
		{"/", examples_action}		
	]}
]}.
```

Action module:
```erlang
-module(examples_action).

-behaviour(kb_action_handler).

-export([handle/3]).

handle(_Method, [], Req) ->
	{html, "<html><body>Kill-Bill examples.</body></html>", Req};

handle(_Method, [<<"obj">>, Id, <<"create">>], Req) ->
	Obj = [{id, Id}, {type, <<"obj">>}],
	{json, Obj, Req};

handle(<<"GET">>, [<<"cities">>], Req) ->
	CitiesList = [<<"Lisbon">>, <<"London">>, <<"Madrid">>],
	Args = [{cities, ValueList}],
	{dtl, cities_dtl, Args, Req};

handle(_Method, [<<"google">>], Req) ->
	{redirect, <<"http://www.google.com">>, Req};

handle(_Method, _Path, Req) ->
	{raw, 404, [], "Not found!", Req}.
```


kb_action_helper Helper module
------------------------------

The module ```kb_action_helper``` provides functions to interact with Kill-Bill.

### API

```erlang
%% Retrive WebApp context
kb_action_helper:get_context(Req :: #kb_request{}) -> binary().

%% Retrive args from request
kb_action_helper:get_args(Req :: #kb_request{}) -> {Args, Req1 :: #kb_request{}}
	when Args :: [{Key :: binary(), Value :: binary()}, ...].

%% Retrive JSON from request body
kb_action_helper:get_json(Req :: #kb_request{}) -> {Doc :: jsondoc:jsondoc(), Req1 :: #kb_request{}}.

%% Retrive data from session
kb_action_helper:get_session(Req :: #kb_request{}) -> {SessionData, Req1 :: #kb_request{}}
	when SessionData :: [{Key :: any(), Value :: any()}, ...].

%% Store data in session
kb_action_helper:set_session(SessionData, Req :: #kb_request{}) -> Req1 :: #kb_request{}
	when SessionData :: [{Key :: any(), Value :: any()}, ...].

%% Invalidate session
kb_action_helper:invalidate_session(Req :: #kb_request{}) -> #kb_request{}.
```

*The module ```kb_action_helper``` has many more functions.*


Kill-Bill tags
--------------

Kill-Bill provides two tags to be use on templates:

### TAG context

Without parameters, returns the WebApp's context
```
<form action="{% context %}action" method="post">
...
</form>
```

With the "file" parameter, the TAG prefixes the path to the file with the WebApp's context:
```
<img src="{% context file="/images/kill-bill-logo.png" %}">
```

### TAG message

Retrieves the localized message for "key" from the messages file (defined in the WebApp configuration file):
```
<h1>{% message key="page_title" %}</h1>
```

### Configuration

To use Kill-Bill's tags, add the following to your project's rebar.config:

```erlang
{erlydtl_opts, [{custom_tags_modules, [kb_dtl_tag]}]}.
```
