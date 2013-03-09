-module(torps).
-behavior(gen_server).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

%%%----------------------------------------------------------------------
%%%
%%% Each live torpedo is tracked by an instance of this module/process.
%%%
%%%----------------------------------------------------------------------

init({Player, TicksRemaining}) ->
	{ok, {Player, TicksRemaining}}.

handle_info(tick, {Player, 0}) ->	
	{stop, normal, {Player, 0}};

handle_info(tick, {Player, TicksRemaining}) ->
	{noreply, {Player, TicksRemaining - 1}};

handle_info(dead, State) ->
	{stop, normal, State};

handle_info({hit, HitPlayer}, {Player, _}) ->
	case HitPlayer of
		HitPlayer when HitPlayer == Player ->
			gen_server:cast(space_score, {Player, -1}),
			{stop, normal, {Player, 0}};
		HitPlayer ->
			gen_server:cast(space_score, {Player, 1}),
			{stop, normal, {Player, 0}}
	end;

handle_info(_, State) ->
	{no_reply, State}.

handle_cast(Message, State) ->
	lager:warning("Unhandled message in torp cast:  ~w", [Message]),
	{noreply, State}.

handle_call(Message, From, State) ->
	lager:warning("Unhandled message in torp call:  ~w", [Message]),
	{reply, unhandled, State}.

code_change(OldVersion, State, Extra) ->
	{ok, State}.

terminate(normal, {Player, _}) ->
	lager:info("Shutting down torp", []),
	gen_fsm:send_event(Player, dead_torp),
	gen_server:cast(play_space, {dead_torp, self()}),
	ok.