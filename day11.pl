:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [sum_list/2]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(yall), [(>>)/3]).

% Dynamic predicate for edge adjacency
:- dynamic adj/2.

% Parser -- we just assert the edges as we see them
parse_graph -->
    parse_name(Name), ":", blanks, sequence(parse_name, Neighbours), eol, blanks,
    { maplist({Name}/[Nb]>>assertz(adj(Name, Nb)), Neighbours) },
    parse_graph.
parse_graph --> eos.
parse_name(Name) -->
    string_without(": \n", NameString),
    { length(NameString, Len), Len > 0, atom_codes(Name, NameString) },
    whites.

% Number of paths in DAG from A to B
:- table num_paths/3.
num_paths(A, A, 1).
num_paths(A, B, Num) :-
    findall(M, (adj(A, Nb), num_paths(Nb, B, M)), Nums),
    sum_list(Nums, Num).

% Number of paths in DAG containing A,B,C,D in order
num_paths(A, B, C, D, Num) :-
    num_paths(A, B, N1), num_paths(B, C, N2), num_paths(C, D, N3), Num is N1 * N2 * N3.

main :-
    retractall(adj(_, _)), 
    current_input(Stdin),
    phrase_from_stream(parse_graph, Stdin),
    num_paths(you, out, Part1),
    num_paths(svr, dac, fft, out, Part2a),
    num_paths(svr, fft, dac, out, Part2b),
    Part2 is Part2a + Part2b,
    format("~w~n~w~n", [Part1, Part2]),
    halt.
