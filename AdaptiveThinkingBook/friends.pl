likes(john, pizza).
likes(john, sushi).
likes(mary, sushi).
likes(mary, salad).

friends(X, Y) :- likes(X, Food), likes(Y, Food), X = Y.
