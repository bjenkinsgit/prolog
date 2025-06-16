% vending.pl
:- module(vending, [init/0,
                    insert_coin/2,
                    current_credit/1]).

:- dynamic credit/1.

%% init
%  Reset the machine’s credit to 0¢.
init :-
    retractall(credit(_)),
    assert(credit(0)).

%% coin_value(+Coin, -ValueCents)
%  Defines accepted coins.
coin_value(nickel,  5).
coin_value(dime,   10).
coin_value(quarter,25).

%% insert_coin(+Coin, -Response)
%  Insert a coin. Response is either
%    ok(Credit)   — not yet enough for soda, returns new total
%    soda         — credit reached ≥250¢, soda dispensed and reset
insert_coin(Coin, Response) :-
    % Validate coin
    coin_value(Coin, Value),
    % Update stored credit
    (   retract(credit(Old)),
        New is Old + Value,
        assert(credit(New))
    ->  true
    ;   % first insertion ever
        New = Value,
        assert(credit(New))
    ),
    (   New >= 250
    ->  % dispense soda
        retractall(credit(_)),
        assert(credit(0)),
        Response = soda
    ;   % still collecting
        Response = ok(New)
    ).

%% current_credit(-Cents)
%  Query how many cents are currently stored.
current_credit(C) :-
    credit(C).
