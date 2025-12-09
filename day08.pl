:- autoload(library(lists), [nth0/3, clumped/2, reverse/2]).
:- autoload(library(dcg/basics), [integer/3, blanks/2]).
:- autoload(library(dcg/high_order) ).
:- autoload(library(apply), [foldl/4, maplist/3]).
:- autoload(library(assoc), [get_assoc/3, put_assoc/4, list_to_assoc/2, assoc_to_keys/2]).
:- use_module(library(yall), [(>>)/4]).
:- autoload(library(pairs), [transpose_pairs/2]).
:- autoload(library(pure_input), [phrase_from_stream/2]).

parse_(X1-X2-X3) --> integer(X1), ",", integer(X2), ",", integer(X3), blanks.
parse(Ps) --> sequence(parse_, Ps).

graph_edge(List, D-X-Y) :-
    nth0(X, List, X1-X2-X3),
    nth0(Y, List, Y1-Y2-Y3),
    X > Y,
    D is (X1-Y1)^2 + (X2-Y2)^2 + (X3-Y3)^2.

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

uf_component_sizes(UF, ComponentSizes) :-
    uf(_, Parent) = UF,
    assoc_to_keys(Parent, Domain),
    maplist({UF}/[X, Y]>>uf_find(UF, X, Y-_, _), Domain, Components),
    msort(Components, SortedComponents),
    clumped(SortedComponents, ComponentSizes).

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
fold_part1(_-A-B, N-UF, M-UF1) :-
    N > 0,
    M is N-1,
    uf_union(UF, A, B, UF1).

fold_part2(_, A-B-uf(1, P), A-B-uf(1, P)).
fold_part2(_-A-B, _-_-UF, A-B-UF1) :-
    uf(N, _) = UF, N > 1,
    uf_union(UF, A, B, UF1).

top_3_product(UF, N) :-
    uf_component_sizes(UF, ComponentSizes),
    transpose_pairs(ComponentSizes, SizeComponents),
    keysort(SizeComponents, S),
    reverse(S, [A-_,B-_,C-_|_]),
    N is A*B*C.

main :-
    current_input(Stdin), phrase_from_stream(parse(Parsed), Stdin),
    findall(Z, graph_edge(Parsed, Z), Edges),
    findall(X, nth0(X, Parsed, _), Indexes),
    keysort(Edges , SortedEdges),
    list_to_uf(Indexes, UF),
    foldl(fold_part1, SortedEdges, 1000-UF, 0-FinalUF),
    top_3_product(FinalUF, Part1), write(Part1), nl,
    foldl(fold_part2, SortedEdges, _-_-UF, I-J-_),
    nth0(I, Parsed, X1-_-_),
    nth0(J, Parsed, X2-_-_),
    Part2 is X1*X2, write(Part2), nl, halt.
