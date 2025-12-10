:- use_module(library(lists), [nth0/3, clumped/2, reverse/2]).
:- use_module(library(dcg/basics), [integer/3, blanks/2]).
:- use_module(library(dcg/high_order) ).
:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(assoc), [get_assoc/3, put_assoc/4, list_to_assoc/2, assoc_to_keys/2]).
:- use_module(library(yall), [(>>)/4]).
:- use_module(library(pairs), [transpose_pairs/2]).
:- use_module(library(pure_input), [phrase_from_stream/2]).

coord(c(X1, X2, X3)) --> integer(X1), ",", integer(X2), ",", integer(X3), blanks.
parse_coords(Coords) --> sequence(coord, Coords).

sorted_edges(Coords, Edges) :-
    findall(D-IdxX-IdxY,
            (
                nth0(IdxX, Coords, c(X1, X2, X3)),
                nth0(IdxY, Coords, c(Y1, Y2, Y3)),
                IdxX > IdxY,
                D is (X1-Y1)^2 + (X2-Y2)^2 + (X3-Y3)^2
            )
           , WeightedEdges),
    keysort(WeightedEdges, SortedEdges),
    maplist([_-U-V,U-V]>>(true), SortedEdges, Edges).

% Union-find data structure based on AVL trees
list_to_uf(List, uf(L, Parent)) :-
    length(List, L),
    maplist([X,X-(X-1)]>>true, List, Pairs),
    list_to_assoc(Pairs, Parent).

uf_find(uf(N, Parent), A, A-Rank, uf(N, Parent)) :-
    get_assoc(A, Parent, A-Rank).
uf_find(uf(N, Parent), A, Root-Rank, uf(N, Parent1)) :-
    get_assoc(A, Parent, P-_),
    A \= P,
    uf_find(uf(N, Parent), P, Root-Rank, uf(_, Tmp)),
    put_assoc(A, Tmp, Root-Rank, Parent1).

uf_union(UF, A, B, UF2) :-
    uf_find(UF, A, Root-_, UF1),
    uf_find(UF1, B, Root-_, UF2).

uf_union(UF, A, B, uf(M, Parent1)) :-
    uf_find(UF, A, RootA-RankA, UF1),
    uf_find(UF1, B, RootB-RankB, uf(N, Parent) ),
    RootA \= RootB,
    M is N-1,
    (
        RankA = RankB -> Root = RootA, Rank is 1+RankA
    ;   RankA < RankB -> Root-Rank = RootB-RankB 
    ;   RankA > RankB -> Root-Rank = RootA-RankA
    ),
    put_assoc(RootA, Parent, Root-Rank, Tmp),
    put_assoc(RootB, Tmp, Root-Rank, Parent1).

fold_part1(_, 0-UF, 0-UF).
fold_part1(A-B, N-UF, M-UF1) :-
    N > 0,
    M is N-1,
    uf_union(UF, A, B, UF1).

top_3_product(UF, N) :-
    uf(_, Parent) = UF,
    assoc_to_keys(Parent, Domain),
    maplist({UF}/[X, Y]>>uf_find(UF, X, Y-_, _), Domain, Components),
    msort(Components, SortedComponents),
    clumped(SortedComponents, ComponentSizes),
    transpose_pairs(ComponentSizes, SizeComponents),
    keysort(SizeComponents, S),
    reverse(S, [A-_,B-_,C-_|_]),
    N is A*B*C.

fold_part2(_, A-B-uf(1, P), A-B-uf(1, P)).
fold_part2(A-B, _-_-UF, A-B-UF1) :-
    uf(N, _) = UF, N > 1,
    uf_union(UF, A, B, UF1).

main :-
    current_input(Stdin), phrase_from_stream(parse_coords(Coords), Stdin),
    sorted_edges(Coords, SortedEdges),
    findall(X, nth0(X, Coords, _), Indexes),
    list_to_uf(Indexes, UF),
    foldl(fold_part1, SortedEdges, 1000-UF, 0-FinalUF),
    top_3_product(FinalUF, Part1),
    foldl(fold_part2, SortedEdges, _-_-UF, I-J-_),
    nth0(I, Coords, c(X1, _, _)),
    nth0(J, Coords, c(X2, _, _)),
    Part2 is X1*X2,
    format("~w~n~w~n", [Part1, Part2]),
    halt.
