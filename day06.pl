:- autoload(library(dcg/basics), [blanks/2, eos/2, integer/3, string_without/4]).
:- autoload(library(clpfd), [transpose/2]).
:- autoload(library(apply), [maplist/4, foldl/4, maplist/3]).
:- use_module(library(yall), [(>>)/5, (>>)/4]).
:- autoload(library(lists), [sum_list/2, reverse/2]).
:- autoload(library(pure_input), [phrase_from_stream/2]).

times(A, B, C) :- C is A * B.
plus(A, B, C) :- C is A + B.

parse_lines([Line|Lines]) -->
    string_without("\n", Line), "\n", parse_lines(Lines).
parse_lines([]) --> eos.

parse_part1([N|Ns]) --> blanks, integer(N), blanks, parse_part1(Ns).
parse_part1([times-1|Ops]) --> "*", blanks, parse_part1(Ops).
parse_part1([plus-0|Ops]) --> "+", blanks, parse_part1(Ops).
parse_part1([]) --> eos.

parse_part2(N) --> blanks, integer(N), blanks.
parse_part2(N-times) --> blanks, integer(N), blanks, "*".
parse_part2(N-plus) --> blanks, integer(N), blanks, "+".
parse_part2(0-plus) --> blanks.

part2_fold(Row, Acc-_-Block, Acc1-Op1-Block1) :-
    phrase(parse_part2(Parsed), Row),
    Parsed = Block1-Op1,
    Acc1 is Acc + Block.
part2_fold(Row, Acc-Op-Unit, Acc-Op-Unit1) :-
    phrase(parse_part2(Num), Row),
    call(Op, Unit, Num, Unit1).

main :-
    current_input(Stdin),
    phrase_from_stream(parse_lines(Lines), Stdin),
    maplist([Line, Row]>>(phrase(parse_part1(Row), Line)), Lines, AllRows),
    reverse(AllRows, [Ops|Rows]),
    transpose(Rows, RowsT),
    maplist([Op-Unit, Row, X]>>(foldl(Op, Row, Unit, X)), Ops, RowsT, Ns),
    sum_list(Ns, Part1),
    transpose(Lines, LinesT),
    foldl(part2_fold, LinesT, 0-plus-0,  A-_-B),
    Part2 is A + B,
    format("~w~n~w~n", [Part1, Part2]),
    halt.
