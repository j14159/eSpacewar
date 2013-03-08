-module(player).
-behavior(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4,
	dead_player/2, dead_player/3, live_player/2, live_player/3]).
-compile([{parse_transform, lager_transform}]).

init({Master, {Xsize, Ysize, TorpLifespan, TorpLimit}}) ->
	random:seed(erlang:now()),
	{X, Y, Z} = starting_pos(Xsize, Ysize),
	Vector = movement:startMatrix(0, 0),
	update_myself(Master, X, Y, Z, Vector),
	{ok, live_player, {Master, {X, Y, Z}, Vector, none, 0, {Xsize, Ysize, TorpLifespan, TorpLimit}}}.

%% we don't actually handle any synchronous calls:
dead_player(Event, From, Data) ->
	lager:warning("Synchronous call made to dead player, not handled:  ~w", [Event]),
	{next_state, dead_player, Data}.

dead_player(respawn, {Master, _, _, _, _, Config}) ->
	{Xsize, Ysize, _, _} = Config,
	{X, Y, Z} = starting_pos(Xsize, Ysize),
	gen_server:cast(play_space, {self(), X, Y, Z, movement:startMatrix(0, 0) }),
	{next_state, live_player, {Master, {X, Y, Z}, movement:startMatrix(0, 0), none, 0, Config}};

dead_player({moved, _, _, NotMe, Torps}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, payload(NotMe, Torps)},
	{next_state, dead_player, Data};

dead_player({score, Score}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, score_payload(Score)},
	{next_state, dead_player, Data};

dead_player(die, Data) ->
	gen_server:cast(play_space, {dead, self()}),
	{stop, player_disconnection, Data};

%% catch-all
dead_player(_, Data) ->
	{next_state, dead_player, Data}.

%% no synchronous calls:
live_player(Event, From, Data) ->
	lager:warning("Synchronous call made to live player, not handled:  ~w", [Event]),
	{next_state, live_player, Data}.

live_player({moved, X1, Y1, NotMe, Torps}, {Master, {X, Y, Z}, Vector, _, LiveTorps, Config}) ->
	Master ! {updated, payload({X1, Y1, Z}, NotMe, Torps)},
	update_myself(Master, X1, Y1, Z, Vector),
	{next_state, live_player, {Master, {X1, Y1, Z}, Vector, none, LiveTorps, Config}};

live_player(thrust, {Master, {X, Y, Z}, Vector, none, LiveTorps, Config}) ->
	{{_, _}, {NvX, NvY}} = movement:addMatrix(Vector, movement:nextMatrix(scaled, Z, 1, X, Y)),
	Vec = {{X, Y}, {NvX, NvY}},
	gen_server:cast(play_space, {self(), X, Y, Z, Vec}),
	%update_myself(Master, X, Y, Z, Vector),
	{next_state, live_player, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}};

live_player(thrust, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}) ->
	update_myself(Master, X, Y, Z, Vector),
	{next_state, live_player, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}};

live_player(torp, {Master, {X, Y, Z}, Vector, Thrust, LiveTorps, Config}) ->
	{_, _, TorpLifespan, TorpLimit} = Config,
	%update_myself(Master, X, Y, Z, Vector),

	case LiveTorps of
		LiveTorps when LiveTorps =< TorpLimit ->
			Torp = spawn(torps, torp, [self(), TorpLifespan]),
			TorpVec = movement:nextMatrix(torp, Z, 8, X, Y),
			{Tx1, Ty1} = move(X, Y, TorpVec, 4),
			
			gen_server:cast(play_space, {torp, {Torp, Tx1, Ty1, 0, TorpVec}}),
			{next_state, live_player, {Master, {X, Y, Z}, Vector, Thrust, LiveTorps + 1, Config}};
		LiveTorps ->
			{next_state, live_player, {Master, {X, Y, Z}, Vector, Thrust, LiveTorps, Config}}
	end;

live_player(dead_torp, {Master, Me, Vector, UpdateVector, LiveTorps, Config}) ->
	{X, Y, Z} = Me,
	%update_myself(Master, X, Y, Z, Vector),
	{next_state, live_player, {Master, Me, Vector, UpdateVector, LiveTorps - 1, Config}};

live_player({attitude, Change}, {Master, {X, Y, Z}, Vector, UpdateVector, LiveTorps, Config}) ->
	%update_myself(Master, X, Y, Z, Vector),
	case Change of
		C when C < 0 ->
			{next_state, live_player, {Master, {X, Y, movement:clampAttitude(Z - 2)}, Vector, UpdateVector, LiveTorps, Config}};
		C when C > 0 ->
			{next_state, live_player, {Master, {X, Y, movement:clampAttitude(Z + 2)}, Vector, UpdateVector, LiveTorps, Config}};
		_ ->
			{next_state, live_player, {Master, {X, Y, Z}, Vector, UpdateVector, LiveTorps, Config}}
	end;

live_player(dead, {Master, Me, _, _, _, Config}) ->
	timer:send_after(2000, respawn),
	{next_state, dead_player, {Master, Me, none, dead, 0, Config}};

live_player({score, Score}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, score_payload(Score)},
	{next_state, live_player, Data};

live_player(die, Data) ->
	gen_server:cast(play_space, {dead, self()}),
	{stop, player_disconnection, Data};

%% catchall
live_player(_, Data) ->
	{next_state, live_player, Data}.

handle_event(Event, StateName, Data) ->
	lager:warning("async player handle_event got a message:  ~w", [Event]),
	{next_state, StateName, Data}.

handle_sync_event(Event, From, StateName, Data) ->
	lager:warning("sync player handle_event got a message:  ~w", [Event]),
	{next_state, StateName, Data}.

handle_info(respawn, _, {Master, _, _, _, _, Config}) ->
	{Xsize, Ysize, _, _} = Config,
	{X, Y, Z} = starting_pos(Xsize, Ysize),
	gen_server:cast(play_space, {self(), X, Y, Z, movement:startMatrix(0, 0) }),
	{next_state, live_player, {Master, {X, Y, Z}, movement:startMatrix(0, 0), none, 0, Config}}.

code_change(OldVersion, StateName, Data, Extra) ->
	{ok, StateName, Data}.

terminate(_, _, _) ->
	lager:info("terminating player ~w", [self()]),
	ok.

update_myself(Master, X, Y, Heading, Vector) ->
	Payload = mochijson2:encode({struct, [{player, entity_struct(X, Y, Heading)}]}),
	Master ! {updated, Payload},
	gen_server:cast(play_space, {self(), X, Y, Heading, Vector}).

starting_pos(Xsize, Ysize) ->
	case random:uniform(1000) of
		S when S =< 250 ->
			{random:uniform(Xsize), (Ysize / 2) - 10, 90};
		S when S =< 500 ->
			{random:uniform(Xsize), 0 - (Ysize / 2) + 10, 270};
		S when S =< 750 ->
			{random:uniform(Ysize), (Xsize / 2) - 10, 0};
		S ->
			{random:uniform(Ysize), 0 - (Xsize / 2)  + 10, 0}
	end.


% TODO:  think I can remove this, move is a bit of a hack to make sure torps spawn far enough away from the player.
move(X, Y, _, 0) ->
	{X, Y};
move(X, Y, Vec, Count) ->
	{X2, Y2} = movement:move({X, Y}, Vec),
	move(X2, Y2, Vec, Count - 1).

entity_struct(X, Y, Z) ->
	{struct, [{x, X}, {y, Y}, {z, Z}]}.

group_struct(Group) ->
	Compacted = [{X, Y, Z} || {_, X, Y, Z, _} <- Group],
	lists:map(fun({X, Y, Z}) -> entity_struct(X, Y, Z) end, Compacted).

% used when a player is dead/inactive:
payload(NotMe, Torps) ->
	Struct = {struct, [{enemies, group_struct(NotMe)}, {torps, group_struct(Torps)}]},
	mochijson2:encode(Struct).

payload(Me, NotMe, Torps) ->
	{MyX, MyY, MyZ} = Me,
	Struct = {struct, [{player, entity_struct(MyX, MyY, MyZ)},
				{enemies, group_struct(NotMe)}, {torps, group_struct(Torps)}]},
	mochijson2:encode(Struct). 

score_payload(Score) ->
	Structified = lists:map(fun({N, S}) -> {struct, [{name, N}, {score, S}]} end, Score),
	FullStruct = {struct, [{score, Structified}]},
	mochijson2:encode(FullStruct).

