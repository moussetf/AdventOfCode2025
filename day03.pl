:- use_module(library(yall)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).

codes_digits(Codes, Digits) :-
    maplist([X,Y]>>(Y is X-48), Codes, Digits).

parse([Bank|Banks]) -->
    digits(Codes),
    {
        length(Codes, D), D > 1, codes_digits(Codes, Bank)
    },
    blanks,
    parse(Banks).
parse([]) --> eos.

% U is the joltage for a battery bank B, where exactly K batteries
% can be turned on.
joltage(Bank, K, U) :-
    reverse(Bank, BankRev),
    findall(I-B, nth0(I, BankRev, B), IndexedRev),
    reverse(IndexedRev, Indexed),
    joltage_(Indexed, K, U).

joltage_(_, 0, 0).
joltage_(Indexed, K, U) :-
    K > 0,
    foldl(maximum, Indexed, K-0-0, _-L-U0),
    append(_, Suffix, Indexed),
    length(Suffix, L),
    K1 is K-1,
    joltage_(Suffix, K1, U1),
    U is 10^(K-1)*U0+U1.

% Auxiliary predicate for the fold
maximum(I-_, K-J-Max, K-J-Max) :- I+1 < K.
maximum(I-B, K-_-Max, K-I-B) :- I+1 >= K, B > Max.
maximum(I-B, K-J-Max, K-J-Max) :- I+1 >= K, Max >= B.

main :-
    current_input(Stdin), phrase_from_stream(parse(Banks), Stdin),
    maplist([B, Z]>>joltage(B, 2, Z), Banks, Part1s),
    sum_list(Part1s, Part1),
    maplist([B, Z]>>joltage(B, 12, Z), Banks, Part2s),
    sum_list(Part2s, Part2),
    write(Part1), nl, write(Part2), nl, halt.

