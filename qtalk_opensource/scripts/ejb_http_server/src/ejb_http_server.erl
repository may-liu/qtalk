-module(ejb_http_server).

-include("logger.hrl").

-export([start/0]).

start() ->
    lager:start(),
    application:start(inets),
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(p1_xml),
  	application:start(ejb_http_server),
	gen_server:cast(whereis(ejb_server),start_cowboy),
    ok.
