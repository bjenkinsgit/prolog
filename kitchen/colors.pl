:- use_module(library(clpfd)).

shirt_colors([red-1, blue-2, green-3]).

solve_shirts(Alice, Bob, Charlie) :-
    % Each person is a number representing a color
    Vars = [Alice, Bob, Charlie],
    Vars ins 1..3,

    % All must wear different colors
    all_different(Vars),

    % Alice cannot wear blue (color 2)
    Alice #\= 2,

    % Label to assign actual values
    label(Vars),

    % Optional: map color names for display
    shirt_colors(Colors),
    memberchk(red-RedVal, Colors),
    memberchk(blue-BlueVal, Colors),
    memberchk(green-GreenVal, Colors),
    maplist(color_name(Colors), Vars, [AliceColor, BobColor, CharlieColor]),

    format("Alice: ~w~n", [AliceColor]),
    format("Bob: ~w~n", [BobColor]),
    format("Charlie: ~w~n", [CharlieColor]).

% Helper to get color name
color_name(Colors, Value, Name) :-
    member(Name-Value, Colors).
