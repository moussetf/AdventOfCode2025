:- use_module(library(clpfd)).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(lists), [member/2]).
:- use_module(library(dcg/high_order), [sequence/4]).
:- use_module(library(yall)).
:- use_module(library(ordsets)).

dots(N) --> ".", { N #= M+1 }, dots(M).
dots(0) --> [].

parse(Start, Splitters) -->
    dots(Start), "S", dots(_), "\n",
    sequence([Ss]>>(parse_splitters_line(Ss, 0)), Splitters).
parse_splitters_line([], _) --> dots(_), "\n".
parse_splitters_line([S|Ss], P) -->
    dots(Q), "^", { S is P+Q, P1 is S+1 },  parse_splitters_line(Ss, P1).

part1_fold(Splitters, Beams-Count, Beams1-Count1) :-
    ord_intersection(Splitters, Beams, HitSplitters),
    ord_subtract(Beams, HitSplitters, PassThroughBeams),
    findall(B, (member(S, HitSplitters), (B is S-1; B is S+1)), Bs),
    list_to_ord_set(Bs, NewBeams),
    ord_union(PassThroughBeams, NewBeams, Beams1),
    length(HitSplitters, Hits),
    Count1 is Count + Hits.

:- table part2/3.
part2(_, [], 1).
part2(Beam, [Ss|Splitters], Count) :-
    (
        member(Beam, Ss)
    ->  Beam1 is Beam-1,
        Beam2 is Beam+1,
        part2(Beam1, Splitters, Count1),
        part2(Beam2, Splitters, Count2),
        Count is Count1+Count2
    ;   part2(Beam, Splitters, Count)
    ).

main :-
    current_input(Stdin), phrase_from_stream(parse(Start, Splitters), Stdin),
    foldl(part1_fold, Splitters, [Start]-0, _-Part1),
    write(Part1), nl,
    part2(Start, Splitters, Part2),
    write(Part2), nl, halt.
