:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(pure_input), [phrase_from_stream/2]).

parse([A-B|Ranges]) --> digits(A), "-", digits(B), ",", parse(Ranges).
parse([A-B]) --> digits(A), "-", digits(B), blanks.

part1 :-
    current_input(Stdin), phrase_from_stream(parse(Ranges), Stdin),
    findall(X, invalid_number(Ranges, 2, X), Xs),
    list_to_set(Xs, Set),
    sum_list(Set, Sum),
    write(Sum), nl, halt.

part2 :-
    current_input(Stdin), phrase_from_stream(parse(Ranges), Stdin),
    findall(X, invalid_number(Ranges, _, X), Xs),
    list_to_set(Xs, Set),
    sum_list(Set, Sum),
    write(Sum), nl, halt.

% Either Codes splits perfectly into K pieces and Fst is the first
% piece (as a number), or it doesn't, and Fst is the power of 10
% that is as long as the first piece should be.
first(Codes, K, Fst) :-
    length(Codes, NCodes),
    L #= div(NCodes, K),
    (
        NCodes #= K*L ->
            prefix(FstCodes, Codes),
            length(FstCodes, L),
            number_codes(Fst, FstCodes)
    ;   Fst #= 10^L
    ).

% Ls is the K-fold concatenation of L with itself.
repeat(L, K, Ls) :- repeat_(L, K, Ls, []).
repeat_(_, 0) --> [].
repeat_(L, K) --> {K #> 0, K1 #= K-1}, L, repeat_(L, K1).

% True if N is an invalid number (with multiplicity K) for one of the
% given ranges.
invalid_number(Ranges, K, N) :-
    member(A-B, Ranges),
    length(B, MaxLen), between(2, MaxLen, K),
    first(A, K, FstA),
    first(B, K, FstB),
    between(FstA, FstB, Piece),
    number_codes(Piece, PieceCodes),
    repeat(PieceCodes, K, Codes),
    number_codes(N, Codes),
    number_codes(NumA, A),
    number_codes(NumB, B),
    between(NumA, NumB, N).
