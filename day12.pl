:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [sum_list/2, member/2, min_list/2]).
:- use_module(library(apply), [maplist/2, maplist/3, include/3]).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(yall), [(>>)/3, (>>)/4]).
:- use_module(library(assoc), [get_assoc/3, put_assoc/4, empty_assoc/1]).
:- use_module(library(backcomp), [flush/0]).

% Parser
parse(Shapes, Regions) -->
    sequence(parse_shape, Shapes),
    sequence(parse_region, Regions).
parse_region(region(Width, Height, Counts)) -->
    integer(Width), "x", integer(Height), ": ",
    sequence(integer, " ", Counts), blanks.
parse_shape(shape(Number, Coords)) -->
    integer(Number), ":", blanks, parse_coords(Coords, c(0,0)).
parse_coords([c(X,Y)|Coords], c(X,Y)) -->
    "#", { X1 is X + 1 }, parse_coords(Coords, c(X1, Y)).
parse_coords(Coords, c(X, Y)) -->
    ".", { X1 is X + 1 }, parse_coords(Coords, c(X1, Y)).
parse_coords(Coords, c(X, Y)) -->
    eol, { X > 0, Y1 is Y + 1 }, parse_coords(Coords, c(0, Y1)).
parse_coords([], c(0, _)) --> eol.

% Basic transformations of shapes
flipY(shape(Number, Coords1), shape(Number, Coords2)) :-
    maplist([c(X, Y1), c(X, Y2)]>>(Y2 is -Y1), Coords1, Coords2).
flipX(shape(Number, Coords1), shape(Number, Coords2)) :-
    maplist([c(X1, Y), c(X2, Y)]>>(X2 is -X1), Coords1, Coords2).
rot90(shape(Number, Coords1), shape(Number, Coords2)) :-
    maplist([c(X1, Y1), c(X2, Y2)]>>(X2 is -Y1, Y2 is X1), Coords1, Coords2).
translate(DX, DY, shape(Number, Coords1), shape(Number, Coords2)) :-
    maplist({DX, DY}/[c(X1, Y1), c(X2, Y2)]>>(X2 is X1 + DX, Y2 is Y1 + DY), Coords1, Coords2).

% Translate the shape so that the top-left coordinate is c(0, 0) and the coordinate list is sorted.
x(c(X, _), X).
y(c(_, Y), Y).
normalize(S1, shape(Number, NormCoords)) :-
    S1 = shape(_, Coords),
    maplist(x, Coords, Xs), min_list(Xs, MinX),
    maplist(y, Coords, Ys), min_list(Ys, MinY),
    DX is -MinX,
    DY is -MinY,
    translate(DX, DY, S1, shape(Number, Coords1)),
    sort(Coords1, NormCoords).

% Generate all possible variants of a shape
variant_ -->
    ([]; flipX; flipY; flipX, flipY; rot90; rot90, flipX; rot90, flipY; rot90, flipX, flipY),
    normalize.
:- table variant/2.
variant(S, Var) :- setof(V, variant_(S, V), Vars), member(Var, Vars).

place(shape(_, Coords), region(Width, Height, _)) -->
    { maplist({Width, Height}/[c(X, Y)]>>( X < Width, Y < Height ), Coords) },
    foreach(member(C, Coords), place_coord(C)).
place_coord(C, Board1, Board2) :-
    \+ get_assoc(C, Board1, _),
    put_assoc(C, Board1, 1, Board2).

solve_region([], _, _, _, _).
solve_region([_|Shapes], region(Width, Height, [0|Counts]), _, _, Board) :-
    solve_region(Shapes, region(Width, Height, Counts), 0, 0, Board).
solve_region([S|Shapes], region(Width, Height, [Count|Counts]), X, Y, Board) :-
    Count > 0,
    Width > X, X >= 0,
    Height > Y, Y >= 0,
    X1 is mod(X + 1, Width),
    Y1 is Y + (X + 1) // Width,
    (
        Count1 is Count - 1,
        variant(S, Var),
        translate(X, Y, Var, TS),
        place(TS, region(Width, Height, [Count|Counts]), Board, Board1)
    ;   (Count1 = Count, Board1 = Board)
    ),
    solve_region([S|Shapes], region(Width, Height, [Count1|Counts]), X1, Y1, Board1).

solve(Shapes, Region) :-
    Region = region(Width, Height, Count),
    % Stupid check that sorts out all the impossible cases...
    sum_list(Count, CountSum),
    Width * Height >= 7*CountSum,
    empty_assoc(Board),
    solve_region(Shapes, Region, 0, 0, Board).

main :-
    current_input(Stdin),
    phrase_from_stream(parse(Shapes, Regions), Stdin),
    include({Shapes}/[R]>>( write('.'), flush, solve(Shapes, R) ), Regions, SolvableRegions),
    nl,
    length(SolvableRegions, Sol),
    format("~w~n", [Sol]),
    halt.
