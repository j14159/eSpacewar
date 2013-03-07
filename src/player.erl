-module(player).
-export([player/2]).

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

player(Master, Config) ->
	io:format("Player PID is ~w~n", [self()]),
	{Xsize, Ysize, _, _} = Config,
	random:seed(erlang:now()),
	{X, Y, Z} = starting_pos(Xsize, Ysize),
	player(Master, {X, Y, Z}, movement:startMatrix(0, 0), none, 0, Config).

player(Master, _, _, dead, _, Config) ->
	{Xsize, Ysize, _, _} = Config,
	receive
		respawn ->
			{X, Y, Z} = starting_pos(Xsize, Ysize),
			gen_server:cast(play_space, {self(), X, Y, Z, movement:startMatrix(0, 0) }),
			player(Master, { X, Y, 0 }, movement:startMatrix(0, 0), none, 0, Config);
		{moved, _, _, NotMe, Torps} ->
			Master ! {updated, payload(NotMe, Torps)},
			player(Master, none, none, dead, 0, Config);
		{score, Score} ->
			Master ! {updated, score_payload(Score)},
			player(Master, {0, 0, 0}, none, dead, 0, Config);
		die ->
			gen_server:cast(play_space, {dead, self()}),
			0;
		_ ->
			player(Master, {0, 0, 0}, none, dead, 0, Config)
	end;

player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps, Config) ->
	{Xsize, Ysize, TorpLifespan, TorpLimit} = Config,

	Payload = mochijson2:encode({struct, [{player, entity_struct(X, Y, Heading)}]}),
	Master ! {updated, Payload},
	gen_server:cast(play_space, {self(), X, Y, Heading, Vector}),
	
	receive
		{moved, X1, Y1, NotMe, Torps} ->
			Master ! {updated, payload({X, Y, Heading}, NotMe, Torps)},
			player(Master, {X1, Y1, Heading}, Vector, none, LiveTorps, Config);
		thrust when UpdateVector =:= none ->
			{{_, _}, {NvX, NvY}} = movement:addMatrix(Vector, movement:nextMatrix(scaled, Heading, 1, X, Y)),
			Vec = {{X, Y}, {NvX, NvY}},
			gen_server:cast(play_space, {self(), X, Y, Heading, Vec}),
			player(Master, {X, Y, Heading}, Vector, done, LiveTorps, Config);
		torp when LiveTorps =< TorpLimit ->
			Torp = spawn(torps, torp, [self(), TorpLifespan]),
			TorpVec = movement:nextMatrix(torp, Heading, 8, X, Y),
			{Tx1, Ty1} = move(X, Y, TorpVec, 4),
			
			gen_server:cast(play_space, {torp, {Torp, Tx1, Ty1, 0, TorpVec}}),
			player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps + 1, Config);
		torp ->
			player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps, Config);
		dead_torp ->
			player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps - 1, Config);
		{attitude, Change} ->
			case Change of
				C when C < 0 ->
					player(Master, {X, Y, movement:clampAttitude(Heading - 2)},
						Vector, UpdateVector, LiveTorps, Config);
				C when C > 0 ->
					player(Master, {X, Y, movement:clampAttitude(Heading + 2)},
						Vector, UpdateVector, LiveTorps, Config);
				_ ->
					player(Master, {X, Y, Heading},
						Vector, UpdateVector, LiveTorps, Config)
			end;
		dead ->
			timer:send_after(2000, respawn),
			player(Master, {0, 0, 0}, none, dead, 0, Config);
		{score, Score} ->
			Master ! {updated, score_payload(Score)},
			player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps, Config);
		die ->
			gen_server:cast(play_space, {dead, self()}),
			0;
		_ ->
			player(Master, {X, Y, Heading}, Vector, UpdateVector, LiveTorps, Config)
	end.

% move is a bit of a hack to make sure torps spawn far enough away from the player.
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

