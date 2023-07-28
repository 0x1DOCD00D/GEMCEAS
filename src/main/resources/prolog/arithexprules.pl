% sign should be minus if the first term in the product div LIST is > 10
sum_sub(_, SecondProductDivList) :-
    nonvar(SecondProductDivList) ->
        (nth1(1, SecondProductDivList, FirstItem),
        FirstItem) ;
        true.

second_product_div(Sign, ProductDiv) :-
    arg(2, ProductDiv, Term),
    arg(1, Term, Number),
    Sign == "-" -> 
        Number > 100 ; 
        true.


% number to the rhs must be > 0 if the sign is "/"
product_div(_, _, SecondTermList) :-
    nonvar(SecondTermList) ->
        loop_over_list(SecondTermList) ;
        true.

loop_over_list([]).
loop_over_list([H | T]) :-
    H,
    loop_over_list(T).

second_term(SecondTermSign, SecondTerm) :-
    arg(1, SecondTerm, Number),
    SecondTermSign == "/" ->
        Number =\= 0 ;
        true.
    

% all numbers must be > 5
term(Number) :-
    Number > 5.
