-module(spacewar1).
-export([start/0]).

start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(spacewar1).
