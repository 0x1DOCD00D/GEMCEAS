% Simple Prolog examples

% if / case statements
what_grade(5) :-
  write('Go to kindergarten').

what_grade(6) :-
  write('Go to 1st grade').

what_grade(Other) :-
  Grade is Other - 5,
  format('Go to grade ~w', [Grade]).



% get balance of a customer using 'customer(tom, _, Bal).'
customer(tom, smith, 20.55).
customer(sally, smith, 120.55).

% custom predicate to print the balance
get_cust_bal(FName, LName) :-
  customer(FName, LName, Bal),
  write(FName), tab(1),
  format('~w owes us $~2f ~n', [LName, Bal]).



% recursion
parent(albert, bob).
parent(albert, betsy).
parent(albert, bill).

parent(alice, bob).
parent(alice, betsy).
parent(alice, bill).

parent(bob, carl).
parent(bob, charlie).

related(X, Y) :-
  parent(X, Y).

related(X, Y) :-
  parent(X, Z),
  related(Z, Y).



% double any digit 'X' and set it to 'Y'
double_digit(X, Y) :-
  Y is X*2.



% add 'albert' to the start of the list
% write([albert | [alice, bob]]), nl.

% saves the length of the list to X
% length([1, 2, 3], X).

% sets X2 to b, and the tail T to an empty list
% [_, X2, _, _ | T] = [a, b, c, d].

% checks if 'a' is a member of 'List1'
% List1 = [a, b, c].
% member(a, List1).



% print out a list
write_list([]).

write_list([H|T]) :-
write(H), nl,
write_list(T).
