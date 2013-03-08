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
	%% let the client and playing space know we're here:
	update_myself(Master, X, Y, Z, Vector),
	{ok, live_player, {Master, {X, Y, Z}, Vector, none, 0, {Xsize, Ysize, TorpLifespan, TorpLimit}}}.

%% we don't actually handle any synchronous calls:
dead_player(Event, From, Data) ->
	lager:warning("Synchronous call made to dead player, not handled:  ~w", [Event]),
	{next_state, dead_player, Data}.

dead_player({moved, _, _, NotMe, Torps}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, payload(NotMe, Torps)},
	{next_state, dead_player, Data};

dead_player({score, Score}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, score_payload(Score)},
	{next_state, dead_player, Data};

%% websocket disconnection:
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

%% playing space has been updated, everyone moved
live_player({moved, X1, Y1, NotMe, Torps}, {Master, {X, Y, Z}, Vector, _, LiveTorps, Config}) ->
	Master ! {updated, payload({X1, Y1, Z}, NotMe, Torps)},
	update_myself(Master, X1, Y1, Z, Vector),
	{next_state, live_player, {Master, {X1, Y1, Z}, Vector, none, LiveTorps, Config}};

%% handle a thrust command from the player iff they haven't applied one yet
%% this tick.
live_player(thrust, {Master, {X, Y, Z}, Vector, none, LiveTorps, Config}) ->
	{{_, _}, {NvX, NvY}} = movement:addMatrix(Vector, movement:nextMatrix(scaled, Z, 1, X, Y)),
	Vec = {{X, Y}, {NvX, NvY}},
	gen_server:cast(play_space, {self(), X, Y, Z, Vec}),
	{next_state, live_player, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}};

%% player's already applied thrust this tick, ignore.
live_player(thrust, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}) ->
	update_myself(Master, X, Y, Z, Vector),
	{next_state, live_player, {Master, {X, Y, Z}, Vector, done, LiveTorps, Config}};

%% player is trying to fire a torpedo, check limits and spawn if allowed
live_player(torp, {Master, {X, Y, Z}, Vector, Thrust, LiveTorps, Config}) ->
	{_, _, TorpLifespan, TorpLimit} = Config,
	
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

%% a torpedo has been spent (lifespan or struck something), update in-flight count
live_player(dead_torp, {Master, Me, Vector, UpdateVector, LiveTorps, Config}) ->
	{X, Y, Z} = Me,
	{next_state, live_player, {Master, Me, Vector, UpdateVector, LiveTorps - 1, Config}};

%% heading change requested by player
live_player({attitude, Change}, {Master, {X, Y, Z}, Vector, UpdateVector, LiveTorps, Config}) ->
	case Change of
		C when C < 0 ->
			{next_state, live_player, {Master, {X, Y, movement:clampAttitude(Z - 2)}, Vector, UpdateVector, LiveTorps, Config}};
		C when C > 0 ->
			{next_state, live_player, {Master, {X, Y, movement:clampAttitude(Z + 2)}, Vector, UpdateVector, LiveTorps, Config}};
		_ ->
			{next_state, live_player, {Master, {X, Y, Z}, Vector, UpdateVector, LiveTorps, Config}}
	end;

%% somebody killed us or we ran into something (player or planet)
live_player(dead, {Master, Me, _, _, _, Config}) ->
	timer:send_after(2000, respawn),
	{next_state, dead_player, {Master, Me, none, dead, 0, Config}};

%% score update, let the client know
live_player({score, Score}, Data) ->
	{Master, _, _, _, _, _} = Data,
	Master ! {updated, score_payload(Score)},
	{next_state, live_player, Data};

%% websocket disconnection
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

%% time to respawn, find a new position and update everyone
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

%% helper method to find a starting position and heading.
%% this should really be in space.erl, obviously but it's outside
%% the scope of what I wanted to accomplish for now.
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


%% move is a bit of a hack to make sure torps spawn far enough away from the player.
move(X, Y, _, 0) ->
	{X, Y};
move(X, Y, Vec, Count) ->
	{X2, Y2} = movement:move({X, Y}, Vec),
	move(X2, Y2, Vec, Count - 1).

%% helps format the player's info for JSON serialization
entity_struct(X, Y, Z) ->
	{struct, [{x, X}, {y, Y}, {z, Z}]}.

%% helps format a group of entities (torps, enemies) for 
%% JSON serialization.
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
