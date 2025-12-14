:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [max_member/2, member/2, sum_list/2, reverse/2, nth0/3]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(pairs), [map_list_to_pairs/3, pairs_values/2]).
:- use_module(library(pure_input), [phrase_from_stream/2]).

% Parse coordinates
coord(c(X, Y)) --> integer(X), ",", integer(Y), blanks.
parse_coords(Coords) --> sequence(coord, Coords).

% Turn the coordinates into a polygon (list of line segments)
polygon([C|Coords], Polygon) :- polygon_([C|Coords], C, Polygon, []).
polygon_([C1,C2|Coords], C0) --> [l(C1,C2)], polygon_([C2|Coords], C0).
polygon_([C1], C0) --> [l(C1,C0)].

% Extract all possible rectangles from the coordinates
rectangles(Coords, Rectangles) :-
    findall(
        r(P, Q),
        (nth0(I, Coords, P), nth0(J, Coords, Q), I > J),
        Rectangles
    ).

% Area of a rectangle
rectangle_area(r(c(X1, Y1), c(X2, Y2)), Area) :-
    Area is (1 + abs(X1 - X2)) * (1 + abs(Y1 - Y2)).

% Sort list of rectangles by decreasing area
sort_rectangles_by_area(Rectangles, SortedRectangles) :-
    map_list_to_pairs(rectangle_area, Rectangles, Pairs),
    keysort(Pairs, SortedPairs),
    reverse(SortedPairs, SortedPairs1),
    pairs_values(SortedPairs1, SortedRectangles).

main :-
    current_input(Stdin), phrase_from_stream(parse_coords(Coords), Stdin),
    rectangles(Coords, Rectangles),
    maplist(rectangle_area, Rectangles, Areas),
    max_member(Part1, Areas),
    polygon(Coords, Polygon),
    sort_rectangles_by_area(Rectangles, SortedRectangles),
    member(Rectangle, SortedRectangles),
    polygon_contains_rectangle(Polygon, Rectangle),
    rectangle_area(Rectangle, Part2),
    format("~w~n~w~n", [Part1, Part2]),
    halt.

% The main idea for part 2 is to compute various points of interest
% such that the rectangle is contained in the polygon iff all the
% points of interest are. These points of interest are the corners
% of the rectangle, as well as all neighbours of a traverse intersection
% point of the rectangle with the polygon.

% Does the rectangle fit completely inside the polygon?
polygon_contains_rectangle(Polygon, Rectangle) :-
    forall(rectangle_poi(Polygon, Rectangle, P), inside(Polygon, P)).

rectangle_poi(_, Rectangle, POI) :-
    rectangle_vertex(Rectangle, POI).
rectangle_poi(Polygon, Rectangle, POI) :-
    rectangle_edge(Rectangle, Edge),
    member(LineSegment, Polygon),
    crossing(Edge, LineSegment, P),
    neighbour(P, POI),
    segment_point(Edge, POI).

% Point neighbourhood
neighbour(c(X, Y0), c(X, Y1)) :- Y1 is Y0 - 1; Y1 is Y0 + 1.
neighbour(c(X0, Y), c(X1, Y)) :- X1 is X0 - 1; X1 is X0 + 1.

% Crossing points of two line segments
crossing(Segment1, Segment2, c(X, Y)) :-
    (
        Segment1 = l(c(X, _), c(X, _)), Segment2 = l(c(_, Y), c(_, Y))
    ;   Segment1 = l(c(_, Y), c(_, Y)), Segment2 = l(c(X, _), c(X, _))
    ),
    segment_point(Segment1, c(X, Y)),
    segment_point(Segment2, c(X, Y)).

% Edges of a rectangle
rectangle_edge(r(c(X1, Y1), c(X2, _)), l(c(X1, Y1), c(X2, Y1))).
rectangle_edge(r(c(X1, Y1), c(_, Y2)), l(c(X1, Y1), c(X1, Y2))).
rectangle_edge(r(c(X1, _), c(X2, Y2)), l(c(X1, Y2), c(X2, Y2))).
rectangle_edge(r(c(_, Y1), c(X2, Y2)), l(c(X2, Y1), c(X2, Y2))).

% Vertices of a rectangle
rectangle_vertex(r(P, _), P).
rectangle_vertex(r(_, Q), Q).
rectangle_vertex(r(c(_, Y1), c(X2, _)), c(X2, Y1)).
rectangle_vertex(r(c(X1, _), c(_, Y2)), c(X1, Y2)).

% Does the point lie on the line segment?
segment_point(l(c(X1, Y), c(X2, Y)), c(X, Y)) :-
    max(X1, X2) >= X, X >= min(X1, X2).
segment_point(l(c(X, Y1), c(X, Y2)), c(X, Y)) :-
    max(Y1, Y2) >= Y, Y >= min(Y1, Y2).

% Determine if a point lies in a polygon
angle(c(X,Y), l(c(X1,Y1), c(X2,Y2)), Angle) :-
    UX is X1-X, UY is Y1-Y,
    VX is X2-X, VY is Y2-Y,
    UU is sqrt(UX * UX + UY * UY),
    VV is sqrt(VX * VX + VY * VY),
    Sign is sign(UX * VY - UY * VX), 
    (
        Sign = 0 -> Angle = 0
    ;   Angle is Sign * acos(((UX * VX) + (UY * VY)) / (UU * VV))
    ).

:- table inside/2.
inside(Polygon, P) :-
    member(Edge, Polygon), segment_point(Edge, P), !.
inside(Polygon, P) :-
    maplist(angle(P), Polygon, Angles),
    sum_list(Angles, Angle),
    abs(Angle) > pi.
