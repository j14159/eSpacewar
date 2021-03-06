-module(movement).
-export([radians/1, degrees/1, 
    nextMatrix/3, nextMatrix/5, addMatrix/2, startMatrix/2, 
    move/2, clampVector/1, clampVector/2, clampAttitude/1,
    clampedAddMatrix/3]).

%%%----------------------------------------------------------------------
%%%
%%% "movement" is one of the earliest modules in this project and it 
%%% probably shows.  Functions here assist in calculating vectors based
%%% on current player attitude, assist with upper bounds on vectors and
%%% simple 2x2 matrix addition.
%%%
%%%----------------------------------------------------------------------

radians(Degrees) ->
    3.14159 * Degrees / 180.

degrees(Radians) ->
    Radians * 180 / 3.14159.

startMatrix(X, Y) ->
    {{X, Y}, {X, Y}}.

nextMatrix(Angle, X, Y) ->
    nextMatrix(scaled, Angle, 0.5, X, Y).

nextMatrix(scaled, Angle, Scaling, X, Y) ->
    X2 = 0 + (Scaling * math:cos(radians(Angle))),
    Y2 = 0 + (Scaling * math:sin(radians(Angle))),
    {{X, Y}, {clampVector(X2), clampVector(Y2)}};
    
nextMatrix(torp, Angle, Scaling, X, Y) ->
    X2 = 0 + (Scaling * math:cos(radians(Angle))),
    Y2 = 0 + (Scaling * math:sin(radians(Angle))),
    {{X, Y}, {X2, Y2}}.

clampVector(V) ->
    clampVector(V, 4).

clampVector(V, Limit) ->
    case V of 
    V when V > Limit ->
        Limit;
    V when V < -Limit ->
        -Limit;
    _ ->
        V
    end.

clampAttitude(A) ->
    case A of
	A when A > 360 ->
	    A - 360;
	A when A < 0 ->
	    A + 360;
	_ ->
	    A
    end.

addMatrix({{Ax0, Ay0}, {Ax1, Ay1}}, {{Bx0, By0}, {Bx1, By1}}) ->
    {{Ax0 + Bx0, Ay0 + By0}, {Ax1 + Bx1, Ay1 + By1}}.

clampedAddMatrix({{Ax0, Ay0}, {Ax1, Ay1}}, {{Bx0, By0}, {Bx1, By1}}, Limit) ->
    {{clampVector(Ax0 + Bx0, Limit), clampVector(Ay0 + By0, Limit)}, 
        {clampVector(Ax1 + Bx1, Limit), clampVector(Ay1 + By1, Limit)}}.

move({CurrentX, CurrentY}, Vector) ->
    {_, {NewX, NewY}} = addMatrix({{CurrentX, CurrentY}, {CurrentX, CurrentY}}, Vector),
    {NewX, NewY}.
	