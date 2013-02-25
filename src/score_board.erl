-module(score_board).
-export([score_board/0]).

score_board() ->
	register(space_score, self()),
	score_board([]).

%%-------------------------
%% State = [{PlayerName, PlayerPid, Score}]
%%-------------------------
score_board(State) ->
	receive
		{PlayerName, PlayerPid, WsProcess} ->
			{Status, NewState} = attempt_join(PlayerName, PlayerPid, State),
			case Status of
				not_available -> 
					WsProcess ! not_available,
					score_board(State);
				ok ->
					WsProcess ! ok,
					broadcast_score(NewState),
					score_board(NewState)
			end;
		{remove, PlayerPid} ->
			State2 = without_player(PlayerPid, State),
			broadcast_score(State2),
			score_board(State2);
		{Pid, ScoreChange} ->
			io:format("Got score change ~w for ~w~n", [ScoreChange, Pid]),
			UpdatedState = update_state(Pid, ScoreChange, State),
			broadcast_score(UpdatedState),
			score_board(UpdatedState)
	end.

broadcast_score(State) ->
	CleanScore = [{Name, Score} || {Name, _, Score} <- State],
	lists:map(fun({_, Pid, _}) -> Pid ! {score, CleanScore} end, State).

attempt_join(NewName, NewPid, State) ->
	Existing = [N || {N, _, _} <- State, N == NewName],
	case Existing of
		[H | T] when H == NewName ->
			{not_available, State};
		_ ->
			{ok, [{NewName, NewPid, 0} | State]}
	end.

get_player(PlayerPid, State) ->
	FilteredByPid = [{N, Pid, S} || {N, Pid, S} <- State, Pid == PlayerPid],
	case FilteredByPid of
		[Player | []] -> Player;
		_ -> not_found
	end.

without_player(PlayerPid, State) ->
	[{N, Pid, S} || {N, Pid, S} <- State, Pid /= PlayerPid].

update_state(PlayerPid, ScoreChange, State) ->
	case get_player(PlayerPid, State) of
		{Name, Pid, Score} ->
			UpdatedScore = Score + ScoreChange,
			[{Name, Pid, UpdatedScore} | without_player(PlayerPid, State)];
		not_found ->
			State
	end.
