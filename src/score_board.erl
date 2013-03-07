-module(score_board).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([{parse_transform, lager_transform}]).

%%%----------------------------------------------------------------------
%%%
%%% "score_board" is just what the name suggests, in addition to being 
%%% the central player registry.
%%%
%%%----------------------------------------------------------------------
init([]) ->
	{ok, []}.

%%
%% Adding a player to the score board.
%%
handle_call({PlayerName, WsProcess}, From, State) ->
	case attempt_join(WsProcess, PlayerName, State) of
		{not_available, State} ->
			{reply, not_available, State};
		{ok, PlayerPid, NewState} ->
			NewCount = length(NewState),
			lager:info("Player joined:  ~s", [PlayerName]),
			lager:info("Player count:  ~p", [NewCount]),
			broadcast_score(NewState),
			{reply, {ok, PlayerPid}, NewState}
	end.

%%
%% Removing a player from the score board.
%%
handle_cast({remove, PlayerPid}, State) ->
	case get_player(PlayerPid, State) of
		{PlayerName, _, _} ->
			gen_server:cast(play_space, {dead, PlayerPid}),
			lager:info("Removing player ~s", [PlayerName]);
		not_found ->
			lager:warning("Trying to remove pid ~w but can't find it", [PlayerPid])
	end,

	NewState = without_player(PlayerPid, State),
	lager:info("Removing player for pid ~w, new length ~w", [PlayerPid, length(NewState)]),
	broadcast_score(NewState),
	{noreply, NewState};

%%
%% Someone got a point or crashed, adjust score appropriately.
%%
handle_cast({PlayerPid, ScoreChange}, State) ->
	NewState = update_state(PlayerPid, ScoreChange, State),
	broadcast_score(NewState),
	{noreply, NewState}.

handle_info(Message, State) ->
	lager:error("Scoreboard received message in handle_info:  ~w", [Message]),
	{noreply, State}.

code_change(PreviousVersion, State, Extra) ->
	lager:warning("Code change in score_board", []),
	{ok, State}.

terminate(Reason, State) ->
	%% TODO:  notify remaining players we're shutting down.
	ok.

%%% PRIVATE functions -------------------------

%%
%% sends the current scoreboard to all registered players.
%%
broadcast_score(State) ->
	%% TODO:  gen_server:cast instead of ! once players are gen_server
	CleanScore = [{Name, Score} || {Name, _, Score} <- State],
	lists:map(fun({_, Pid, _}) -> Pid ! {score, CleanScore} end, State).

%%
%% checks to see if the requested name is available for a new player and if so,
%% creates a new player process.
%%
attempt_join(WsPid, NewName, State) ->
	Existing = [N || {N, _, _} <- State, N =:= NewName],
	case Existing of
		[H | _] when H == NewName ->
			{not_available, State};
		_ ->
			{ok, Xsize} = application:get_env(xsize),
			{ok, Ysize} = application:get_env(ysize),
			{ok, TorpLifespan} = application:get_env(torp_lifespan),
			{ok, TorpLimit} = application:get_env(torp_limit),

			NewPid = spawn(player, player, [WsPid, {Xsize, Ysize, TorpLifespan, TorpLimit}]),
			{ok, NewPid, [{NewName, NewPid, 0} | State]}
	end.

%%
%% finds the player corresponding to the given pid in the current score board.
%%
get_player(PlayerPid, State) ->
	FilteredByPid = [{N, Pid, S} || {N, Pid, S} <- State, Pid == PlayerPid],
	case FilteredByPid of
		[Player | []] -> Player;
		_ -> not_found
	end.

%%
%% filters the player corresponding to the given pid out of the score board.
%%
without_player(PlayerPid, State) ->
	[{N, Pid, S} || {N, Pid, S} <- State, Pid /= PlayerPid].

%%
%% updates the score board for the given player and change.
%%
update_state(PlayerPid, ScoreChange, State) ->
	case get_player(PlayerPid, State) of
		{Name, Pid, Score} ->
			UpdatedScore = Score + ScoreChange,
			[{Name, Pid, UpdatedScore} | without_player(PlayerPid, State)];
		not_found ->
			State
	end.
