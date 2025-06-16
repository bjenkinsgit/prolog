zebra_solution(Houses) :-

    initialize_houses(Houses),

    member(house( englishman,   red,           _,    _,       _), Houses),
    member(house(   spaniard,     _,           _,  dog,       _), Houses),
    member(house(          _, green,      coffee,    _,       _), Houses),
    member(house(   ukranian,     _,         tea,    _,       _), Houses),
    member(house(          _,     _,           _,snail, dancing), Houses),
    member(house(          _,     _,        milk,    _,       _), Houses),
    member(house(          _,yellow,           _,    _, painter), Houses),
    member(house(          _,     _,orange_juice,_    ,football), Houses),
    member(house(   japanese,     _,      _,    _,        chess), Houses),

    % ... more constraints
    green_right_of_ivory(Houses),
    at_position(3, Houses, house(_, _, milk, _, _)),
    at_position(1, Houses, house(norwegian, _, _, _, _)),
    
    adjacent(
        house(_, _, _, fox, _),
        house(_, _, _, _, reading),
        Houses
    ),
    adjacent(
        house(_, _, _, _, painter),
        house(_, _, _, horse, _),
        Houses
    ),
    adjacent(
        house(norwegian, _, _, _, _),
        house(_, blue, _, _, _),
        Houses
    ),
    true.

at_position(1, [X|_], X).             % clause 1
at_position(N, [_|T], X) :-           % clause 2
    N > 1,
    N1 is N - 1,
    at_position(N1, T, X).

adjacent(A, B, List) :-
    next_to(A, B, List);
    next_to(B, A, List).

next_to(A, B, List) :-
    append(_, [A, B | _], List).


initialize_houses([
    house(N1, C1, D1, P1, A1),
    house(N2, C2, D2, P2, A2),
    house(N3, C3, D3, P3, A3),
    house(N4, C4, D4, P4, A4),
    house(N5, C5, D5, P5, A5)
]).

green_right_of_ivory(Houses) :-
    append(_, [
        house(_, ivory, _, _, _),
        house(_, green, _, _, _)
   |_], Houses).

zebra_owner(Owner) :-
    zebra_solution(Houses), member(house(Owner, _, _, zebra, _), Houses). 

water_drinker(Drinker) :-
    zebra_solution(Houses), member(house(Drinker, _, water, _, _), Houses). 


