:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pure_input), [phrase_from_stream/2]).

parse([A-B|Ranges]) --> integer(A), "-", integer(B), ",", parse(Ranges).
parse([A-B]) --> integer(A), "-", integer(B), blanks.

main :-
    current_input(Stdin), phrase_from_stream(parse(Ranges), Stdin),
    setof(X, invalid_number(Ranges, 2, X), Part1s),
    sum_list(Part1s, Part1),
    setof(X, K^invalid_number(Ranges, K, X), Part2s),
    sum_list(Part2s, Part2),
    write(Part1), nl, write(Part2), nl, halt.

% Either the decimal representation of N splits perfectly into K pieces and Fst
% is the first piece (interpreted as a number), or it doesn't, and then Fst is
% the power of 10 that is as long as the first piece should be.
first(N, K, Fst) :-
    Length is ceil(log10(N+1)),
    PieceLen is div(Length, K),
    (
        Length is K*PieceLen -> Fst is div(N, 10^((K-1)*PieceLen))
    ;   Fst is 10^PieceLen
    ).

% True if M is the number obtained by K-fold concatenation of N with itself.
repeatN(N, K, M) :- repeatN_(N, K, M, 0).
repeatN_(_, 0, Acc, Acc).
repeatN_(N, K, M, Acc) :-
    K > 0, L is K-1,
    Acc0 is 10^ceil(log10(N+1))*Acc + N,
    repeatN_(N, L, M, Acc0).

% True if N is an invalid number (with multiplicity K) for one of the
% given ranges.
invalid_number(Ranges, K, N) :-
    member(A-B, Ranges),
    KMax is ceil(log10(B+1)), between(2, KMax, K),
    first(A, K, Lower), first(B, K, Upper), between(Lower, Upper, Piece),
    repeatN(Piece, K, N),
    between(A, B, N).
