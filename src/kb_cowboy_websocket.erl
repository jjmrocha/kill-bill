-module(kb_cowboy_websocket).

-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({_TransportName, _ProtocolName}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, [WebApp]) ->
	kb_webapp:client_connect(WebApp),
	{ok, Req, WebApp}.

websocket_handle({text, Msg}, Req, WebApp) ->
	kb_webapp:client_cast(WebApp, Msg),
	{ok, Req, WebApp};
websocket_handle(_Data, Req, WebApp) ->
	{ok, Req, WebApp}.

websocket_info(Msg, Req, WebApp) ->
	{reply, {text, Msg}, Req, WebApp}.

websocket_terminate(_Reason, _Req, WebApp) ->
	kb_webapp:client_disconnect(WebApp),
	ok.