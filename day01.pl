:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(library(pure_input), [phrase_from_stream/2]).

parse(Ns) --> sequence(move, Ns).
move(N) --> "R", integer(N), blanks.
move(N) --> "L", integer(M), {N is -M}, blanks.

% Count the number of times the cumulative sum is zero mod 100.
count_zeroes(Pos, NumZeroes) -->
    [Move],
    { Pos1 is mod(Pos + Move, 100) },
    { Pos1 is 0 -> NumZeroes #= NumZeroes1 + 1 ; NumZeroes = NumZeroes1 },
    count_zeroes(Pos1, NumZeroes1).
count_zeroes(_, 0) --> eos.

% Count the number of times the cumulative sum "passes" through a
% multiple of 100.
count_crossings(Pos, NumCrossings) -->
    [Move],
    { Pos1 is mod(Pos + Move, 100) },
    {
        Move >= 0
    ->  NumCrossings #= NumCrossings1 + div(Pos + Move, 100)
    ;   NumCrossings #= NumCrossings1 + div(mod(100 - Pos, 100) - Move, 100)
    },
    count_crossings(Pos1, NumCrossings1).
count_crossings(_, 0) --> eos.

main :-
    current_input(Stdin),
    phrase_from_stream(parse(Ns), Stdin),
    phrase(count_zeroes(50, Part1), Ns),
    phrase(count_crossings(50, Part2), Ns),
    format("~w~n~w~n", [Part1, Part2]),
    halt.
