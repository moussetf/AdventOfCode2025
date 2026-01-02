:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [sum_list/2, max_list/2, member/2, 
                               min_list/2, list_to_set/2]).
:- use_module(library(apply), [maplist/2, maplist/3, include/3]).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(yall), [(>>)/3, (>>)/4]).
:- use_module(library(aggregate), [foreach/2]).
:- use_module(library(assoc), [get_assoc/3, put_assoc/4, empty_assoc/1]).
:- use_module(library(solution_sequences), [distinct/1]).
:- use_module(library(ordsets), [ord_disjoint/2, ord_union/3]).

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

% Translate the shape so that the top-left coordinate is c(0, 0) and order the coordinate list.
x(c(X, _), X).
y(c(_, Y), Y).
normalize(S1, shape(Number, Coords1)) :-
    S1 = shape(_, Coords),
    maplist(x, Coords, Xs), min_list(Xs, MinX),
    maplist(y, Coords, Ys), min_list(Ys, MinY),
    DX is -MinX,
    DY is -MinY,
    translate(DX, DY, S1, shape(Number, Coords2)),
    sort(Coords2, Coords1).

% All possible variants of a shape
variant_ -->
    ([]; flipX; flipY; flipX, flipY; rot90; rot90, flipX; rot90, flipY; rot90, flipX, flipY),
    normalize.
:- table variant/2.
variant(S, Var) :- setof(V, variant_(S, V), Vars), member(Var, Vars).

place(shape(_, Coords), region(Width, Height, _), Board1, Board2) :-
    maplist({Width, Height}/[c(X, Y)]>>(X < Width, Y < Height), Coords),
    ord_disjoint(Coords, Board1),
    ord_union(Coords, Board1, Board2).


solve_region([_|Shapes], region(Width, Height, [0|Counts]), _, _, Board) :-
    solve_region(Shapes, region(Width, Height, Counts), 0, 0, Board).
solve_region(Shapes, region(Width, Height, [Count|Counts]), Width, Y, Board) :-
    Count > 0,
    Height > Y, Y >= 0,
    Y1 is Y + 1,
    solve_region(Shapes, region(Width, Height, [Count|Counts]), 0, Y1, Board).
solve_region([], _, _, _, _).
solve_region([S|Shapes], region(Width, Height, [Count|Counts]), X, Y, Board) :-
    Count > 0,
    Width > X, X >= 0,
    Height > Y, Y >= 0,
    (Height - Y) * Width - X >= 7 * Count,
    X1 is X + 1,
    (
        variant(S, Var),
        translate(X, Y, Var, TS),
        place(TS, region(Width, Height, [Count|Counts]), Board, Board1),
        Count1 is Count - 1,
        solve_region([S|Shapes], region(Width, Height, [Count1|Counts]), X1, Y, Board1)
    ;   solve_region([S|Shapes], region(Width, Height, [Count|Counts]), X1, Y, Board)
    ).

solve(Shapes, Region) :-
    Region = region(Width, Height, Count),
    sum_list(Count, CountSum),
    Width * Height >= 7*CountSum,
    solve_region(Shapes, Region, 0, 0, []).

main :-
    current_input(Stdin),
    phrase_from_stream(parse(Shapes, Regions), Stdin),
    include({Shapes}/[R]>>( write('.'), flush, solve(Shapes, R) ), Regions, SolvableRegions),
    nl,
    length(SolvableRegions, Sol),
    format("~w~n", [Sol]),
    halt.

% Pretty-print the board
format_region(region(Width, Height, _), Board) :-
    H1 is Height-1,
    W1 is Width-1,
    findall(X-Y, ( between(0, H1, Y), between(0, W1, X) ), Indices),
    maplist(
        {W1,Board}/[X-Y]>>(
            ( member(c(X, Y), Board) ->  format("#", []) ;   format(".", []) ),
            ( X is W1 -> format("~n"); true )
        ),
        Indices
    ),
    format("~n").
