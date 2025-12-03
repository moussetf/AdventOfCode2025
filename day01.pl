:- use_module(library(dcg/basics)).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(pure_input), [phrase_from_stream/2]).

parse([N|Ns]) --> "R", integer(N), blanks, parse(Ns).
parse([N|Ns]) --> "L", integer(M), {N is -M}, blanks, parse(Ns).
parse([]) --> eos.

main :-
    current_input(Stdin),
    phrase_from_stream(parse(Ns), Stdin),
    foldl(count_zeroes, Ns, 50-0, _-Part1),
    foldl(count_crossings, Ns, 50-0, _-Part2),
    write(Part1), nl, write(Part2), nl, halt.

count_zeroes(Move, Pos-Zs, Pos1-Zs1) :-
    Pos1 is mod(Pos + Move, 100),
    (
        Pos1 is 0 -> Zs1 is Zs+1
    ;   Zs1 = Zs
    ).

count_crossings(Move, Pos-Zs, Pos1-Zs1) :-
    Pos1 is mod(Pos + Move, 100),
    (
        Move >= 0 -> Zs1 is Zs + div(Pos + Move, 100)
    ;   Zs1 is Zs + div(mod(100 - Pos, 100) - Move, 100)
    ).
