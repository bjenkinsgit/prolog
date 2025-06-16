:- use_module(library(clpfd)).

dish(pizza , oven , 15,  8).
dish(cookie, oven ,  8,  3).
dish(fries , fryer,  7,  5).
dish(wings , fryer, 12,  6).
dish(burger, grill, 12,  7).
dish(hot_dog, grill, 5,  4).
dish(steak , grill, 20, 12).

best_plan(Shift, Plan, Profit) :-
    %------------------------------------------------------------------
    % 1.  Build four parallel columns
    %------------------------------------------------------------------
    findall(Name   , dish(Name,   _,      _,      _), Names   ),
    findall(Machine, dish(_,      Machine,_,      _), Machines),
    findall(Time   , dish(_,      _,      Time,   _), Times   ),
    findall(Value  , dish(_,      _,      _,      Value), Profits),

    %------------------------------------------------------------------
    % 2.  Decision variables — one Count per dish
    %------------------------------------------------------------------
    length(Names, N),
    length(Counts, N),
    Counts ins 0..10,

    %------------------------------------------------------------------
    % 3.  Per-machine capacity:  Σ Timeᵢ·Countᵢ  ≤  Shift
    %------------------------------------------------------------------
    forall(member(M, [oven, fryer, grill]),
           ( findall(T, (nth1(I, Machines, M), nth1(I, Times,   T)), Coeffs),
             findall(C, (nth1(I, Machines, M), nth1(I, Counts,  C)), Vars  ),
             scalar_product(Coeffs, Vars, #=<, Shift)
           )),

    %------------------------------------------------------------------
    % 4.  Global labour limit:  Σ Timeᵢ·Countᵢ  ≤  2·Shift
    %------------------------------------------------------------------
    scalar_product(Times, Counts, #=<, Shift*2),

    %------------------------------------------------------------------
    % 5.  Objective:  maximise  Σ Profitᵢ·Countᵢ
    %------------------------------------------------------------------
    scalar_product(Profits, Counts, #=, Profit),
    labeling([max(Profit), ffc], Counts),

    %------------------------------------------------------------------
    % 6.  Turn the solution into a readable plan
    %------------------------------------------------------------------
    findall(Name-Count,
            ( nth1(I, Names, Name),
              nth1(I, Counts, Count),
              Count #> 0 ),
            Plan).

%-----------------------------------------------------------
%  Show every plan that achieves the maximum profit
%-----------------------------------------------------------
print_all_plans(Shift) :-
        best_plan(Shift, _Any, Profit),           % first call discovers max Profit
        format("Maximum profit: ~w~n", [Profit]),
        forall(best_plan(Shift, Plan, Profit),    % list each optimal plan
               writeln(Plan)).
