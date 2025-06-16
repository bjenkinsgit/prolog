% Soft Drink Vending Machine in SWI-Prolog
% Author: Claude
% Description: A vending machine that accepts coins and dispenses drinks

:- dynamic(current_amount/1).
:- dynamic(dispense_button_lit/1).

% Initialize the machine state
initialize_machine :-
    retractall(current_amount(_)),
    retractall(dispense_button_lit(_)),
    assert(current_amount(0)),
    assert(dispense_button_lit(false)),
    write('Vending machine initialized. Insert coins to purchase a drink ($1.50).'), nl.

% Coin values in cents
coin_value(nickel, 5).
coin_value(dime, 10).
coin_value(quarter, 25).
coin_value(dollar, 100).

% Required amount for a drink (in cents)
drink_price(150).

% Insert a coin into the machine
insert_coin(CoinType) :-
    coin_value(CoinType, Value),
    current_amount(CurrentAmount),
    NewAmount is CurrentAmount + Value,
    retract(current_amount(CurrentAmount)),
    assert(current_amount(NewAmount)),
    format('Inserted ~w (~w cents). Total: ~w cents~n', [CoinType, Value, NewAmount]),
    check_dispense_button_status,
    display_status.

% Insert multiple coins at once
insert_coins([]).
insert_coins([Coin|Rest]) :-
    insert_coin(Coin),
    insert_coins(Rest).

% Check if dispense button should be lit
check_dispense_button_status :-
    current_amount(Amount),
    drink_price(Price),
    (   Amount >= Price
    ->  (   dispense_button_lit(false)
        ->  retract(dispense_button_lit(false)),
            assert(dispense_button_lit(true)),
            write('*** DISPENSE BUTTON IS NOW LIT ***'), nl
        ;   true
        )
    ;   (   dispense_button_lit(true)
        ->  retract(dispense_button_lit(true)),
            assert(dispense_button_lit(false)),
            write('*** DISPENSE BUTTON IS NO LONGER LIT ***'), nl
        ;   true
        )
    ).

% Display current machine status
display_status :-
    current_amount(Amount),
    drink_price(Price),
    dispense_button_lit(ButtonLit),
    format('Current amount: ~w cents ($~2f)~n', [Amount, Amount/100]),
    format('Price of drink: ~w cents ($~2f)~n', [Price, Price/100]),
    (   Amount >= Price
    ->  Needed is 0
    ;   Needed is Price - Amount
    ),
    format('Amount needed: ~w cents ($~2f)~n', [Needed, Needed/100]),
    format('Dispense button lit: ~w~n', [ButtonLit]),
    nl.

% Push the dispense button
push_dispense_button :-
    dispense_button_lit(ButtonLit),
    (   ButtonLit = true
    ->  dispense_drink
    ;   write('Dispense button is not functional. Please insert more coins.'), nl
    ).

% Dispense the drink and handle change
dispense_drink :-
    current_amount(Amount),
    drink_price(Price),
    Overage is Amount - Price,
    write('*** DISPENSING DRINK ***'), nl,
    (   Overage > 0
    ->  format('Returning change: ~w cents ($~2f)~n', [Overage, Overage/100]),
        return_change(Overage)
    ;   write('No change to return.'), nl
    ),
    % Reset machine state
    retract(current_amount(Amount)),
    assert(current_amount(0)),
    retract(dispense_button_lit(true)),
    assert(dispense_button_lit(false)),
    write('Transaction complete. Thank you!'), nl,
    nl.

% Return change (simplified - just announces the amount)
return_change(Amount) :-
    format('Change dispensed: ~w cents~n', [Amount]).

% More sophisticated change return that breaks down into coins
return_change_detailed(Amount) :-
    calculate_change(Amount, Change),
    write('Change returned: '),
    display_change(Change),
    nl.

% Calculate optimal change breakdown
calculate_change(Amount, Change) :-
    Dollars is Amount // 100,
    Remainder1 is Amount mod 100,
    Quarters is Remainder1 // 25,
    Remainder2 is Remainder1 mod 25,
    Dimes is Remainder2 // 10,
    Nickels is (Remainder2 mod 10) // 5,
    Change = [dollars(Dollars), quarters(Quarters), dimes(Dimes), nickels(Nickels)].

% Display change breakdown
display_change([]) :- nl.
% Display change breakdown
display_change([]) :- nl.
display_change([Coin|Rest]) :-
    Coin =.. [Type, Count],          % split the compound term
    (   Count > 0
    ->  format('~d ~w ', [Count, Type])
    ;   true
    ),
    display_change(Rest).

% Get current machine state
get_current_amount(Amount) :-
    current_amount(Amount).

get_button_status(Status) :-
    dispense_button_lit(Status).

% Utility predicate to show available commands
help :-
    write('Available commands:'), nl,
    write('- initialize_machine.          % Initialize/reset the machine'), nl,
    write('- insert_coin(CoinType).       % Insert nickel, dime, quarter, or dollar'), nl,
    write('- insert_coins([coin1,coin2]). % Insert multiple coins at once'), nl,
    write('- push_dispense_button.        % Try to dispense a drink'), nl,
    write('- display_status.              % Show current machine status'), nl,
    write('- help.                        % Show this help message'), nl,
    nl,
    write('Example usage:'), nl,
    write('?- initialize_machine.'), nl,
    write('?- insert_coin(quarter).'), nl,
    write('?- insert_coins([quarter, quarter, quarter, quarter, quarter, quarter]).'), nl,
    write('?- push_dispense_button.'), nl,
    nl.

% Demo sequence
demo :-
    write('=== VENDING MACHINE DEMO ==='), nl,
    initialize_machine,
    nl,
    write('Inserting 6 quarters ($1.50)...'), nl,
    insert_coins([quarter, quarter, quarter, quarter, quarter, quarter]),
    nl,
    write('Pushing dispense button...'), nl,
    push_dispense_button,
    nl,
    write('Now inserting 2 dollars ($2.00)...'), nl,
    insert_coins([dollar, dollar]),
    nl,
    write('Pushing dispense button...'), nl,
    push_dispense_button,
    write('=== DEMO COMPLETE ==='), nl.

% Initialize the machine when the program loads  
:- initialization(initialize_machine).
