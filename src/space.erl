-module(space).
-export([space/2]).

space(Xsize, Ysize) ->
	register(play_space, self()),
	timer:send_after(50, update),
	space(Xsize, Ysize, [], [], whereis(space_score)).

space(Xsize, Ysize, Players, Torps, ScoreBoard) ->
	receive
		update ->
			{NotSuicided, Suicided} = moved_and_suicides(Players, Xsize, Ysize),
			% kill spent torps and adjust scores:
			{StillTorping, HitTorps, PlanetTorps, Torped} = torping_and_torped(Torps, NotSuicided, Xsize, Ysize),
			
			TorpAndKilled = lists:zip(HitTorps, Torped),

			lists:map(fun({{Pid, _, _, _, _}, {HitShipPid, _, _, _, _}}) -> Pid ! {hit, HitShipPid} end, TorpAndKilled),
			lists:map(fun({Pid, _, _, _, _}) -> Pid ! dead end, PlanetTorps),
			lists:map(fun({Pid, _, _, _, _}) -> Pid ! tick end, StillTorping),
			
			%adjust scores for suicides:
			lists:map(fun({Pid, _, _, _, _}) -> gen_server:cast(space_score, {Pid, -1}) end, Suicided),
				%ScoreBoard ! {Pid, -1} end, Suicided),

			Dead1 = lists:flatten([Suicided | Torped]),
			NotDead1 = filter_dead(Dead1, NotSuicided),

			{NotDead, Dead2} = planet_impacts(20, NotDead1, [], []),
			Dead = lists:flatten([Dead1 | Dead2]),
			
			msg_players(NotDead, NotDead, StillTorping),
			% tell dead players they're dead:
			lists:map(fun({Pid, _, _, _, _}) -> Pid ! dead end, Dead),
			
			timer:send_after(50, update),
			space(Xsize, Ysize, NotDead, StillTorping, ScoreBoard);
		{dead_torp, Pid} ->
			LiveTorps = [{P, X, Y, Z, V} || {P, X, Y, Z, V} <- Torps, P /= Pid],
			space(Xsize, Ysize, Players, LiveTorps, ScoreBoard);
		% a player is removing themselves:
		{dead, Pid} ->
			Filtered = 
				[{P, XX, YY, ZZ, V} || {P, XX, YY, ZZ, V} <- Players, P /= Pid],
			space(Xsize, Ysize, Filtered, Torps, ScoreBoard);
		% a player is updating space with attitude + desired control:
		{Pid, X, Y, Z, Vec} ->
			Filtered = [{P, XX, YY, ZZ, V} || {P, XX, YY, ZZ, V} <- Players, P /= Pid],
			case [{P, XX, YY, ZZ, V} || {P, XX, YY, ZZ, V} <- Players, P == Pid] of
				[{_, _, _, _, V}] ->
					space(Xsize, Ysize, [{Pid, X, Y, Z, movement:clampedAddMatrix(Vec, V, 4)} | Filtered], Torps, ScoreBoard);
				_ ->
					space(Xsize, Ysize, [{Pid, X, Y, Z, Vec} | Filtered], Torps, ScoreBoard)
			end;
		% a player fired a torpedo
		{torp, T} ->
			space(Xsize, Ysize, Players, [T | Torps], ScoreBoard);
		_ ->
			0
	end.

%returns {moved and live players, players who collided with other players}
moved_and_suicides(Players, Xsize, Ysize) ->
	Move = fun({P, X, Y, Z, V}) -> move_entity(P, X, Y, Z, planet_influence(X, Y, V, 8, Xsize, Ysize), Xsize, Ysize) end,
			
	MovedPlayers = lists:map(Move, Players),			
	% now check collisions:
	{Suicided, _} = collisions(MovedPlayers, MovedPlayers, [], []),
			
	% get a list of *live* enemies for display:
	NotSuicided = filter_dead(Suicided, MovedPlayers),
	{NotSuicided, Suicided}.

torping_and_torped(Torps, Players, Xsize, Ysize) ->
	Move = fun({P, X, Y, Z, V}) -> move_entity(P, X, Y, Z, planet_influence(X, Y, V, 8, Xsize, Ysize), Xsize, Ysize) end,
	MovedTorps = lists:map(Move, Torps),
	{Torped, HitTorps} = collisions(Players, MovedTorps, [], []),
	{StillTorping, PlanetTorps} = planet_impacts(20, filter_dead(HitTorps, MovedTorps), [], []),

	{StillTorping, HitTorps, PlanetTorps, Torped}.

% change an entities vector based on planetary gravity.
planet_influence(X, Y, Vec, Mass, SpaceW, SpaceH) ->
	Distance = math:sqrt((X * X) + (Y * Y)),
	PlanetEffect = Mass * (1 / (Distance * Distance)),
	{FullX, FullY} = {0 - X, 0 - Y},
	PlanetVector = {{X, Y}, {PlanetEffect * FullX, PlanetEffect * FullY}},
	movement:addMatrix(Vec, PlanetVector).

% takes a list of entities (ships/torps) and recursively checks to see which
% ones are OK and which ones hit the planet.
planet_impacts(PlanetSize, [], Ok, Dead) ->
	{Ok, Dead};
planet_impacts(PlanetSize, [Entity | Rest], Ok, Dead) ->
	case planet_impact(Entity, PlanetSize) of
		ok ->
			planet_impacts(PlanetSize, Rest, [Entity | Ok], Dead);
		Hit ->
			planet_impacts(PlanetSize, Rest, Ok, [Hit | Dead])
	end.

planet_impact(Entity, PSize) ->
	{_, X, Y, _, _} = Entity,
	case math:sqrt((X * X) + (Y * Y)) of
		D when D < PSize ->
			Entity;
		_ ->
			ok
	end.

% finds all ships involved in collisions, sub-optimal, runs in O(n^2) at best:
collisions([], Players, Dead, ToRemove) ->
	{Dead, ToRemove};
collisions([Ship | Rest], Players, Dead, ToRemove) ->
	NewDead = collision_check(Ship, [P || P <- Players, P /= Ship]),
	case NewDead of 
		{none, []} ->
			collisions(Rest, Players, Dead, ToRemove);
		{Ship, Remove} ->
			collisions(Rest, Players, [Ship | Dead], [Remove | ToRemove])
	end.

% checks an individual ship for collisions against the others:
collision_check(Ship, Others) ->
	{Pid, X, Y, _, _} = Ship,
	Hits = [{P, X2, Y2 ,Z2, V} || {P, X2, Y2, Z2, V} <- Others, abs(X - X2) =< 5, abs(Y - Y2) =< 5],
	case Hits of
		[H | T] ->
			io:format("Impacting object ~w~n", [H]),
			{Ship, H};
		_ ->
			{none, []}
	end.

% filters the list of dead players out of active ones
filter_dead([], Players) ->
	Players;
filter_dead([FirstDead | Rest], Players) ->
	{DeadPid, _, _, _, _} = FirstDead,
	Filtered = [{Pid, X, Y, Z, V} || {Pid, X, Y, Z, V} <- Players, Pid /= DeadPid],
	filter_dead(Rest, Filtered).

% recurses through player list to broadcast enemy locations:
msg_players([], _, _) ->
	[];
msg_players([P | Rest], Players, Torps) ->
	{Pid, X, Y, _, _} = P,
	NotMe = [E || E <- Players, E /= P],
	Msg = {moved, X, Y, NotMe, Torps},
	Pid ! Msg,
	msg_players(Rest, Players, Torps).

move_entity(Pid, X, Y, Z, V, Xsize, Ysize) ->
	{X2, Y2} = movement:move({X, Y}, V),
	{Pid, valid_space(X2, Xsize), valid_space(Y2, Ysize), Z, V}.

% clamps space coordinates FIXME:  magic numbers
valid_space(C, Size) ->
	case C of
		C when C >= (Size / 2) ->
			C - Size;
		C when C < (-Size / 2)->
			C + Size;
		_ ->
			C
	end.
