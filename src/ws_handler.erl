-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-compile([{parse_transform, lager_transform}]).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, _Opts) ->
    {PlayerName, Req2} = cowboy_req:qs_val(<<"player">>, Req),
    SafeName = filter_angle_brackets(PlayerName),
    lager:info("Attempting to log in ~s", [SafeName]),

    case check_empty_username(SafeName) of
        empty ->
            self() ! {updated, mochijson2:encode({struct, [{error, <<"Empty usernames not allowed">>}]})},
            {ok, Req2, undefined};
        ok ->
            Player = spawn(player, player, [self(), 500, 800]),
            check_available_username(SafeName, Player, Req2)
    end.

filter_angle_brackets(Name) ->
    % strip open and close for HTML tags.
    << <<C>> || <<C>> <= Name, C /= 60, C /= 62 >>.

check_empty_username(Name) ->
    case Name of
        <<"">> ->
            empty;
        _ -> ok
    end.

check_available_username(PlayerName, Player, Req) ->
    whereis(space_score) ! {PlayerName, Player, self()},
    receive
        ok -> 
            io:format("Player ~s at PID ~w~n", [PlayerName, Player]),
            {ok, Req, Player};
        not_available ->
            
            lager:warning("Duplicate name caught:  ~s", [PlayerName]),
            Player ! die,
            self() ! {updated, mochijson2:encode({struct, [{error, <<"That's someone else's name, choose a different one">>}]})},
            {ok, Req, undefined}
    end.

websocket_handle({text, Msg}, Req, State) ->
    case Msg of
        <<"1", _, _, _>> ->
            State ! {attitude, -1};
        <<_, "1", _, _>> ->
            State ! {attitude, 1};
        _ -> 0
    end,
    
    case Msg of
        <<_, _, "1", _>> ->
            State ! thrust;
        _ -> 0
    end,
    
    case Msg of
        <<_, _, _, "1">> ->
            State ! torp;
        _ -> 0
    end,
    
    {ok, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({updated, Payload}, Req, State) ->
    {reply, {text, Payload}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("Info got ~w~n", [_Info]),
    {ok, Req, State}.

websocket_terminate(Reason, Req, State) ->
    io:format("Disconnection, removing ~w~n", [State]),
    case State of
        undefined ->
            0;
        _ ->
            State ! die,
            whereis(space_score) ! {remove, State}
    end,
    ok.
