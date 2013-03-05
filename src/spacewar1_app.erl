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
						{"/ws", ws_handler, []},
						{"/[...]", cowboy_static, [
								{directory, {priv_dir, spacewar1, []}},
								{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
								]}
						]}
				]),
	cowboy:start_http(http, 100,
		[{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]
		),
	lager:start(),
	gen_server:start_link({local, space_score}, score_board, [], []),
	
	{ok, Xsize} = application:get_env(xsize),
	{ok, Ysize} = application:get_env(ysize),
	{ok, PlanetSize} = application:get_env(planet_radius),
	{ok, ShipMass} = application:get_env(ship_mass),
	{ok, TorpMass} = application:get_env(torp_mass),

	gen_server:start_link({local, play_space}, space, 
		[Xsize, Ysize, PlanetSize, ShipMass, TorpMass], []),
	spacewar1_sup:start_link().


stop(_State) ->
	ok.
