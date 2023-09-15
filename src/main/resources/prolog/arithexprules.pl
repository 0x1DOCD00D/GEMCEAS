expression(SumSub) :-
    % call sum_sub/2
    SumSub.

sum_sub(_, SecondProductDivList) :-
    nonvar(SecondProductDivList) ->
        (nth1(1, SecondProductDivList, FirstItem),
        FirstItem) ;
        true.

product_div_repetition(Sign, ProductDiv) :-
    arg(2, ProductDiv, Term),
    arg(1, Term, NumberMaybe),
    (number(NumberMaybe) ->
        % term is a num. Ensure the num is > 100 if the sign is '-''
        (Sign == "-" -> 
            NumberMaybe > 100 ; 
            true) ;
        % term is expression/1
        Term).


% number to the rhs must be > 0 if the sign is "/"
product_div(_, _, SecondTermList) :-
    (nonvar(SecondTermList) ->
        loop_over_list(SecondTermList) ;
        true).

loop_over_list([]).
loop_over_list([H | T]) :-
    H,
    loop_over_list(T).

term_repetition(Sign, Term) :-
    arg(1, Term, NumberMaybe), 
    (number(NumberMaybe) ->
        % term is a num. Ensure the num is not 0 if the sign is "/"
        (Sign == "/" ->
            NumberMaybe =\= 0 ;
            true) ;
        % term is expression/1
        Term).
    

term(Number) :-
    Number > -5.

term(_, Expression, _) :-
    Expression.
