:- use_module(library(yall)).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(assoc)).

parse(Rolls) --> { empty_assoc(Assoc) }, parse(Assoc, Rolls, 0-0). 
parse(Rolls0, Rolls1, X-Y) --> ".", { X1 is X+1 }, parse(Rolls0, Rolls1, X1-Y).
parse(Rolls0, Rolls1, X-Y) --> "@", { X1 is X+1, put_assoc(X-Y, Rolls0, _, Assoc) }, parse(Assoc, Rolls1, X1-Y).
parse(Rolls0, Rolls1, _-Y) --> "\n", blanks, { Y1 is Y+1 }, parse(Rolls0, Rolls1, 0-Y1).
parse(Rolls, Rolls, _-_) --> eos.

neighbour(X1-Y, X2-Y) :- X2 is X1+1; X2 is X1-1.
neighbour(X-Y1, X-Y2) :- Y2 is Y1+1; Y2 is Y1-1.
neighbour(X1-Y1, X2-Y2) :- (X2 is X1+1; X2 is X1-1), (Y2 is Y1+1; Y2 is Y1-1).

accessible(Rolls, Roll) :-
    gen_assoc(Roll, Rolls, _),
    findall(Nb, (neighbour(Roll, Nb), get_assoc(Nb, Rolls, _)), NeighbourRolls),
    length(NeighbourRolls, N), N < 4.

remove_accessible(Rolls0, Rolls1, NRemoved) :-
    findall(Roll, accessible(Rolls0, Roll), Accessible),
    foldl([R, Cur, New]>>(del_assoc(R, Cur, _, New)), Accessible, Rolls0, Rolls1),
    length(Accessible, NRemoved).

remove_until_done(Rolls, N, Acc) :-
    remove_accessible(Rolls, Rolls1, N1),
    (N1 is 0 -> N is Acc; Acc1 is Acc + N1, remove_until_done(Rolls1, N, Acc1)).

main :-
    current_input(Stdin), phrase_from_stream(parse(Rolls), Stdin),
    remove_accessible(Rolls, _, Part1), write(Part1), nl,
    remove_until_done(Rolls, Part2, 0), write(Part2), nl, halt.
