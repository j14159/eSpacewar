-module(torps).
-export([torp/2]).

torp(Space, Player) ->
	torp(Space, Player, 100).

torp(Space, Player, 0) ->
	Player ! dead_torp,
	Space ! {dead_torp, self()},
	0;
torp(Space, Player, TicksRemaining) ->
	receive
		tick ->
			torp(Space, Player, TicksRemaining - 1);
		dead ->
			io:format("Torp killed by space~n", []),
			torp(Space, Player, 0);
		hit ->
			io:format("Torp hit~n", []),
			%whereis(space_score) ! {Player, 1},
			gen_server:cast(space_score, {Player, 1}),
			torp(Space, Player, 0);
		_ ->
			torp(Space, Player, TicksRemaining)
	end.
