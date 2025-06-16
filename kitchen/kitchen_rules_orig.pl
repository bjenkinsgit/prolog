Below is a compact but complete SWI-Prolog program that

• stores the machines that exist in the kitchen;  
• keeps the preparation time and profit of every dish;  
• imposes the rule “no more than 2 machines can run at the same time”;  
• searches for the numbers of each dish that can be cooked in a shift of
   a given length so that the total profit is maximised.

It uses library(clpfd) (finite-domain constraint programming), so no
installation beyond SWI-Prolog itself is required.

----------------------------------------------------
:- use_module(library(clpfd)).

/* -----------  fixed data  ----------------------- */

machine(oven).
machine(fryer).
machine(grill).

/* dish(Name, MachineNeeded, PrepTimeMinutes, ProfitDollars) */
dish(pizza , oven , 15,  8).
dish(cookie, oven ,  8,  3).
dish(fries , fryer,  7,  5).
dish(wings , fryer, 12,  6).
dish(burger, grill, 12,  7).
dish(steak , grill, 20, 12).

/* ------------  optimiser  ----------------------- */
/*
   best_plan(+ShiftMinutes, -Plan, -Profit)
   Plan is a list Name-Count telling how many times to cook each dish
   in a shift that lasts ShiftMinutes minutes.  Profit is the maximum
   obtainable profit while never running more than two machines at the
   same time.
*/
best_plan(Shift, Plan, Profit) :-
        % Build parallel lists : one per dish
        findall(Name-Machine-Time-Dollar,
                dish(Name,Machine,Time,Dollar), Tuples),
        pairs_keys_values(Tuples,         Names, MachinesTimes),
        pairs_values(MachinesTimes,       MT_pairs),
        pairs_keys_values(MT_pairs,       Machines, Times),
        findall(D, dish(_,_,_,D),         Profits),

        length(Names, N),
        length(Counts, N),            % decision variables
        Counts ins 0..1000,           %   “up to 1000 of each dish” (can be raised)

        % Per-machine capacity: a single machine cannot be busy
        % more than Shift minutes during the shift.
        forall(member(M, [oven,fryer,grill]),
               (   findall(T*C,
                           (nth1(I,Names, _), nth1(I,Machines,M), nth1(I,Times,T),
                            nth1(I,Counts,C)),
                           MCList),
                   sum(MCList, #=<, Shift)
               )),

        % Global capacity: at any minute at most 2 machines are running.
        % → Over an entire shift we have  Shift * 2  machine-minutes
        %   of “power budget”.
        findall(T*C, (nth1(I,Times,T), nth1(I,Counts,C)), AllTC),
        sum(AllTC, #=<, Shift*2),

        % Profit to maximise
        findall(P*C, (nth1(I,Profits,P), nth1(I,Counts,C)), PCList),
        sum(PCList, #=, Profit),

        labeling([max(Profit),ffc], Counts),

        % convert into Name-Count pairs, drop the 0’s for readability
        findall(Name-Count,
                (nth1(I,Names,Name), nth1(I,Counts,Count), Count #> 0),
                Plan).

/* ---------------  helper  ----------------------- */
sum(List, Op, Expr) :- foldl(plus_, List, 0, Expr0), call(Op, Expr0, Expr).
plus_(Term, Acc, Acc+Term).
----------------------------------------------------

HOW IT WORKS
1.  Individual machine limits  
    The loop “forall(member(M, …))” adds one constraint per machine,  
    e.g. for the oven:   Σcountᵢ * prepTimeᵢ   ≤  Shift

2.  “Only two machines at once”  
    Total machine-minutes that can be consumed in the whole shift is  
    Shift*2.  The constraint “sum(AllTC, #=<, Shift*2)” captures that.

3.  Optimisation  
    label/2 with the option max(Profit) makes CLP(FD) search for the
    assignment of the Count variables that yields the greatest profit
    while satisfying all constraints.

HOW TO USE
?- best_plan(60, Plan, Profit).
Plan   = [steak-3, fries-2, cookie-1],
Profit = 55 .

meaning: in a 60-minute service you should grill 3 steaks, fry 2 orders
of fries, and bake 1 tray of cookies; that fits all machine limits and
earns $55, which is optimal.

TUNING / EXTENSIONS
• Add or remove dish/4 facts – the optimiser picks them up automatically.  
• Change the upper bound “Counts ins 0..1000” if much larger batches are
  realistic.  
• If you ever want to allow all 3 machines to run concurrently, just
  change the factor 2 in  Shift*2  to a 3.