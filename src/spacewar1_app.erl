-module(spacewar1_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
				{'_', [
						{"/", test_handler, []},
						{"/ws", ws_handler, []},
						{"/sw/[...]", cowboy_static, [
								{directory, {priv_dir, spacewar1, []}},
								{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
								]}
						]}
				]),
	cowboy:start_http(http, 100,
		[{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]
		),
	spawn(score_board, score_board, []),
	spawn(space, space, [800, 500]),	
	spacewar1_sup:start_link().


stop(_State) ->
	ok.
