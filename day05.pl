:- use_module(library(yall)).
:- use_module(library(apply), [partition/4 ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input), [phrase_from_stream/2]).
:- use_module(library(random), [random_permutation/2]).
:- use_module(library(lists), [append/3, list_to_set/2]).

parse([A-B|Intervals], Ids) --> integer(A), "-", integer(B), blanks, parse(Intervals, Ids).
parse([], [Id|Ids]) --> integer(Id), { Id >= 0 }, blanks, parse([], Ids).
parse([], []) --> eos.

random_search_tree(List, Tree) :-
    set_random(seed(42)),
    random_permutation(List, Shuffled),
    search_tree(Shuffled, Tree).
search_tree([Root|List], tree(Root, Left, Right)) :-
    partition({Root}/[X]>>(X<Root), List, LeftList, RightList),
    search_tree(LeftList, Left),
    search_tree(RightList, Right).
search_tree([], leaf).

range_query(_, _, leaf, []).
range_query(From, To, tree(Root, Left, Right), [Root|Range]) :-
    To >= Root, Root >= From, 
    range_query(From, To, Left, Range1),
    range_query(From, To, Right, Range2),
    append(Range1, Range2, Range).
range_query(From, To, tree(Root, Left, _), Range) :-
    To < Root, range_query(From, To, Left, Range).
range_query(From, To, tree(Root, _, Right), Range) :-
    From > Root, range_query(From, To, Right, Range).

count_fresh_part1(Intervals, Tree, Count) :- 
    gen_fresh(Intervals, Tree, Combined, []),
    list_to_set(Combined, Fresh),
    length(Fresh, Count).
gen_fresh([From-To|Intervals], Tree) --> 
    { range_query(From, To, Tree, Range) },
    Range,
    gen_fresh(Intervals, Tree).
gen_fresh([], _) --> [].

count_fresh_part2([], 0).
count_fresh_part2([A-B], Count) :- Count is 1+B-A.
count_fresh_part2([A-B,A1-B1|Intervals], Count) :-
    B >= A1, B < B1,
    count_fresh_part2([A-B1|Intervals], Count).
count_fresh_part2([A-B,A1-B1|Intervals], Count) :-
    B >= A1, B >= B1,
    count_fresh_part2([A-B|Intervals], Count).
count_fresh_part2([A-B,A1-B1|Intervals], Count) :-
    A1 > B,
    count_fresh_part2([A1-B1|Intervals], Count1),
    Count is Count1+1+B-A.

main :-
    current_input(Stdin),
    phrase_from_stream(parse(Intervals, Ids), Stdin),
    random_search_tree(Ids, Tree),
    count_fresh_part1(Intervals, Tree, Part1),
    sort(Intervals, Sorted), count_fresh_part2(Sorted, Part2),
    format("~w~n~w~n", [Part1, Part2]),
    halt.
