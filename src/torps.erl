-module(torps).
-export([torp/1]).

torp(Player) ->
	torp(Player, 100).

torp(Player, 0) ->
	Player ! dead_torp,
	gen_server:cast(play_space, {dead_torp, self()}),
	0;
torp(Player, TicksRemaining) ->
	Me = self(),
	receive
		tick ->
			torp(Player, TicksRemaining - 1);
		dead ->
			io:format("Torp killed by space~n", []),
			torp(Player, 0);
		{hit, Player} ->
			%% this is a suicide
			gen_server:cast(space_score, {Player, -1}),
			torp(Player, 0);
		{hit, WhoDidWeHit} ->
			io:format("Torp hit~n", []),
			%whereis(space_score) ! {Player, 1},
			gen_server:cast(space_score, {Player, 1}),
			torp(Player, 0);
		_ ->
			torp(Player, TicksRemaining)
	end.
