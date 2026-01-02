:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_symdiff/3]).
:- use_module(library(yall), [(>>)/4]).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [min_member/2, nth0/3, sum_list/2, 
                               member/2]).
:- use_module(library(simplex)).

% Parser
% Each machine is represented as a functor m(Lights, Buttons, Joltages), where:
%  - Lights is a list of indexes that are initially turned on;
%  - Buttons is a list of lists of indexes that are toggled by the button;
%  - Joltages is a list of numbers (one for each index).
parse_machines(Machines) --> sequence(machine, Machines).
machine(mach(Lights, Buttons, Joltages)) --> goal(Lights), buttons(Buttons), joltages(Joltages).
goal(Lights) --> "[", inner_goal(Lights, 0), blanks.
inner_goal([N|Lights], N) --> "#", {M is N + 1}, inner_goal(Lights, M).
inner_goal(Lights, N) --> ".", {M is N + 1}, inner_goal(Lights, M).
inner_goal([], _) --> "]".
buttons(Buttons) --> sequence(button, Buttons).
button(Button) --> "(", sequence(integer, ",", List), ")", blanks, { list_to_ord_set(List, Button) }.
joltages(Joltages) --> "{", sequence(integer, ",", Joltages), "}", blanks.

% True if the configuration of lights can be achieved with exactly N
% button toggles.
solve_part1(Lights, [B|Buttons], N) :-
    M #= N - 1,
    ord_symdiff(Lights, B, NewLights),
    solve_part1(NewLights, Buttons, M).
solve_part1(Lights, [_|Buttons], N) :- solve_part1(Lights, Buttons, N).
solve_part1([], [], 0).

shortest_solution_part1(mach(Lights, Buttons, _), N) :-
    findall(M, solve_part1(Lights, Buttons, M), Ms),
    min_member(N, Ms).

% We use the linear programming solver for part 2.
solution_part2(Machine, Sol) :-
    gen_state(State0),
    part2_lp(Machine, State0, State1),
    objective(State1, Sol).

part2_lp(mach(_, Buttons, Joltages)) -->
    { length(Buttons, Length), L is Length - 1, findall(p(Idx), between(0, L, Idx), NumPresses) },
    foreach(member(P, NumPresses), constraint([P] >= 0)),
    foreach(member(P, NumPresses), constraint(integral(P))),
    button_presses_match(Buttons, Joltages, 0),
    minimize(NumPresses).

button_presses_match(Buttons, [J|Joltages], Current) -->
    {
        findall(p(Idx), nth0(Idx, Buttons, [Current|_]), Joltage),
        maplist({Current}/[As, Bs]>>(As = [Current|Rest] -> Bs = Rest; Bs = As), Buttons, NewButtons),
        Next is Current + 1
    },
    constraint(Joltage = J),
    button_presses_match(NewButtons, Joltages, Next).
button_presses_match(_, [], _, S, S).

main :-
    current_input(Stdin),
    phrase_from_stream(parse_machines(Machs), Stdin),
    maplist(shortest_solution_part1, Machs, Sols1), sum_list(Sols1, Part1),
    maplist(solution_part2, Machs, Sols2), sum_list(Sols2, Part2),
    format("~w~n~w~n", [Part1, Part2]),
    halt.
