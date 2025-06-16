:- use_module(library(lists)).
:- use_module(library(clpz)).   % -------- CLP(Z) --------

:- meta_predicate forall(0,0).          % --- add these two
forall(Generate, Test) :-               %     lines only once
    \+ (Generate, \+ Test).

%----------------------------------------------------------- data
dish(pizza , oven , 15,  8).
dish(cookie, oven ,  8,  3).
dish(fries , fryer,  7,  5).
dish(wings , fryer, 12,  6).
dish(burger, grill, 12,  7).
dish(steak , grill, 20, 12).

% give every Var in a list the domain 0‥10
counts_domain([]).
counts_domain([V|Vs]) :-
    V in 0..10,
    counts_domain(Vs).

%----------------------------------------------------------- main entry
best_plan(Shift, Plan, Profit) :-
    % 1. parallel columns
    findall(Name   , dish(Name,   _,      _,      _), Names   ),
    findall(Machine, dish(_,      Machine,_,      _), Machines),
    findall(Time   , dish(_,      _,      Time,   _), Times   ),
    findall(Value  , dish(_,      _,      _,      Value), Profits),

    % 2. decision variables
    length(Names, N),
    length(Counts, N),
    counts_domain(Counts),          % <── replaces “Counts ins 0..10”

    % 3. per-machine capacity
    forall(member(M, [oven, fryer, grill]),
           ( findall(T, (nth1(I, Machines, M), nth1(I, Times,   T)), Coeffs),
             findall(C, (nth1(I, Machines, M), nth1(I, Counts,  C)), Vars  ),
             scalar_product(Coeffs, Vars, #=<, Shift)
           )),

    % 4. global labour limit
    scalar_product(Times, Counts, #=<, Shift*2),

    % 5. profit equation
    scalar_product(Profits, Counts, #=, Profit),

    % 6. optimise: maximise Profit
    NegProfit #= -Profit,
    label([NegProfit|Counts]), 

    % 7. readable plan
    findall(Name-Count,
            ( nth1(I, Names, Name),
              nth1(I, Counts, Count),
              Count #> 0 ),
            Plan).

print_all_plans(Shift) :-
        best_plan(Shift, _Any, Profit),
        format("Maximum profit: ~w~n", [Profit]),
        forall(best_plan(Shift, Plan, Profit),
               writeln(Plan)).
