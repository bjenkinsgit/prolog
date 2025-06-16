final([Final], Final) :- write("one").

final([First | Rest], Final) :-
   write("two"),
   final(Rest, Final).


