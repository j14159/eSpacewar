-module(ws_handler).
-behaviour(cowboy_websocket_handler).

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
            check_available_username(SafeName, Req2)
    end.

websocket_handle({text, Msg}, Req, State) ->
    case Msg of
        <<"1", _, _, _>> ->
            gen_fsm:send_event(State, {attitude, -1});
        <<_, "1", _, _>> ->
            gen_fsm:send_event(State, {attitude, 1});
        _ -> Msg
    end,
    
    case Msg of
        <<_, _, "1", _>> ->
            gen_fsm:send_event(State, thrust);
        _ -> Msg
    end,
    
    case Msg of
        <<_, _, _, "1">> ->
            gen_fsm:send_event(State, torp);
        _ -> Msg
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
    lager:info("Disconnection, removing ~w", [State]),
    case State of
        undefined ->
            lager:warning("undefined State in disconnection", []),
            0;
        _ ->
            lager:info("State in disconnection is OK"),
            gen_fsm:send_event(State, die),
            gen_server:cast(space_score, {remove, State})
    end,
    ok.

%%
%% strips out < and > in case user's submitted HTML/scripts.
%%
filter_angle_brackets(Name) ->
    << <<C>> || <<C>> <= Name, C /= 60, C /= 62 >>.

check_empty_username(Name) ->
    case Name of
        <<"">> ->
            empty;
        _ -> ok
    end.

check_available_username(PlayerName, Req) ->
    case gen_server:call(space_score, {PlayerName, self()}) of
        {ok, Player} -> 
            io:format("Player ~s at PID ~w~n", [PlayerName, Player]),
            {ok, Req, Player};
        not_available ->                
            lager:warning("Duplicate name caught:  ~s", [PlayerName]),
            self() ! {updated, mochijson2:encode({struct, [{error, <<"That's someone else's name, choose a different one">>}]})},
            {ok, Req, undefined}
    end.

