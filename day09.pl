:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [max_member/2, member/2, sum_list/2, reverse/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(pairs), [map_list_to_pairs/3, pairs_values/2]).
:- use_module(library(pure_input), [phrase_from_stream/2]).

% Parse coordinates
coord(c(X, Y)) --> integer(X), ",", integer(Y), blanks.
parse_coords(Coords) --> sequence(coord, Coords).

% Turn the coordinates into a polygon (list of line segments)
polygon([C1|Coords], Polygon) :- polygon_([C1|Coords], C1, Polygon, []).
polygon_([C1,C2|Coords], C0) --> [l(C1,C2)], polygon_([C2|Coords], C0).
polygon_([C1], C0) --> [l(C1,C0)].

% Extract all possible rectangles from the coordinates
rectangles(Coords, Rectangles) :-
    findall(
        Rect,
        (
            member(c(X1, Y1), Coords),
            member(c(X2, Y2), Coords),
            ( X1 = X2, Y1 > Y2; X1 > X2 ),
            Rect = r(c(X1, Y1), c(X2, Y2))
        ),
        Rectangles
    ).

% Area of a rectangle
area(r(c(X1, Y1), c(X2, Y2)), Area) :-
    Area is (1 + abs(X1 - X2)) * (1 + abs(Y1 - Y2)).

% Sort list of rectangles by decreasing area
sort_rectangles_by_area(Rectangles, SortedRectangles) :-
    map_list_to_pairs(area, Rectangles, Pairs),
    keysort(Pairs, SortedPairs),
    reverse(SortedPairs, SortedPairs1),
    pairs_values(SortedPairs1, SortedRectangles).

main :-
    current_input(Stdin), phrase_from_stream(parse_coords(Coords), Stdin),
    rectangles(Coords, Rectangles),
    maplist(area, Rectangles, Areas),
    max_member(Part1, Areas),
    polygon(Coords, Polygon),
    sort_rectangles_by_area(Rectangles, SortedRectangles),
    member(Rectangle, SortedRectangles),
    inside(Polygon, Rectangle),
    area(Rectangle, Part2),
    format("~w~n~w~n", [Part1, Part2]),
    halt.

% Does the rectangle fit completely inside the polygon?
inside(Polygon, Rectangle) :-
    \+ ( rectangle_poi(Polygon, Rectangle, POI), outside(Polygon, POI) ).

% "Points of interest" for a rectangle are the corners and all points
% adjacent to a crossing with the polygon. If all of the
% points of interest are contained in the polygon, then the rectangle
% is also inside.
rectangle_poi(_, Rectangle, POI) :-
    rect_vertex(Rectangle, POI).
rectangle_poi(Polygon, Rectangle, POI) :-
    rect_line(Rectangle, Line1),
    member(Line2, Polygon),
    crossing_neighbour(Line1, Line2, POI).

crossing_neighbour(l(c(XA1, YA), c(XA2, YA)), l(c(XB, YB1), c(XB, YB2)), c(X, YA)) :- 
    max(XA1, XA2) >= XB, XB >= min(XA1, XA2),
    max(YB1, YB2) >= YA, YA >= min(YB1, YB2),
    (X is XB - 1; X is XB + 1),
    max(XA1, XA2) >= X, X >= min(XA1, XA2).
crossing_neighbour(l(c(XA, YA1), c(XA, YA2)), l(c(XB1, YB), c(XB2, YB)), c(XA, Y)) :- 
    max(XB1, XB2) >= XA, XA >= min(XB1, XB2),
    max(YA1, YA2) >= YB, YB >= min(YA1, YA2),
    (Y is YB - 1; Y is YB + 1),
    max(YA1, YA2) >= Y, Y >= min(YA1, YA2).

rect_line(r(c(X1, Y1), c(X2, _)), l(c(X1, Y1), c(X2, Y1))).
rect_line(r(c(X1, Y1), c(_, Y2)), l(c(X1, Y1), c(X1, Y2))).
rect_line(r(c(X1, _), c(X2, Y2)), l(c(X1, Y2), c(X2, Y2))).
rect_line(r(c(_, Y1), c(X2, Y2)), l(c(X2, Y1), c(X2, Y2))).

rect_vertex(r(C1, _), C1).
rect_vertex(r(_, C2), C2).
rect_vertex(r(c(_, Y1), c(X2, _)), c(X2, Y1)).
rect_vertex(r(c(X1, _), c(_, Y2)), c(X1, Y2)).

angle(c(X,Y), l(c(X1,Y1), c(X2,Y2)), Angle) :-
    \+ on_line(c(X,Y), l(c(X1,Y1), c(X2,Y2))),
    UX is X1-X, UY is Y1-Y,
    VX is X2-X, VY is Y2-Y,
    UU is sqrt(UX * UX + UY * UY),
    VV is sqrt(VX * VX + VY * VY),
    Sign is sign(UX * VY - UY * VX), 
    (Sign = 0 -> Angle = 0;
     Angle is Sign * acos(((UX * VX) + (UY * VY)) / (UU * VV))).

on_line(c(X,Y), l(c(X1,Y), c(X2,Y))) :-
    max(X1, X2) >= X, X >= min(X1, X2).
on_line(c(X,Y), l(c(X,Y1), c(X,Y2))) :-
    max(Y1, Y2) >= Y, Y >= min(Y1, Y2).

:- table outside/2.
outside(Lines, C) :-
    maplist(angle(C), Lines, Angles),
    sum_list(Angles, Angle),
    abs(Angle) < pi.
