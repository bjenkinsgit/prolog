dog(fido).
dog(rover).
dog(tom).
dog(fred).

cat(mary).
cat(jane).
cat(harry).
cat(henry).
cat(bill).
cat(steve).

large(fido).
large(mary).
large(fred).
large(henry).
large(steve).
large(jim).
large(mike).

small(rover).
small(jane).
small(tom).
large_dog(X) :- dog(X), large(X).
small_animal(A) :- dog(A), small(A).
small_animal(B) :- cat(B), small(B).
chases(X,Y) :-
   large_dog(X), small_animal(Y),
   write(X), write(' chases '), write(Y), nl.
