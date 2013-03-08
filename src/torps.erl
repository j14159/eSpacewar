-module(torps).
-export([torp/2]).

%%%----------------------------------------------------------------------
%%%
%%% Each live torpedo is tracked by an instance of this module/process.
%%%
%%%----------------------------------------------------------------------

torp(Player, 0) ->
	gen_fsm:send_event(Player, dead_torp),
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
			%% if a player hits themself with their own torpedo,
			%% it's considered a suicide.
			gen_server:cast(space_score, {Player, -1}),
			torp(Player, 0);
		{hit, WhoDidWeHit} ->
			io:format("Torp hit~n", []),
			gen_server:cast(space_score, {Player, 1}),
			torp(Player, 0);
		_ ->
			torp(Player, TicksRemaining)
	end.
