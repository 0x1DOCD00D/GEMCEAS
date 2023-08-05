:- consult('../../main/resources/prolog/arithexprules.pl').

:- begin_tests(arithexprules).


test("if sum_sub's sign is '-' the first term of the second product div should be > 100") :-
    sum_sub(_, [second_product_div("-", product_div(_, number(120), _))]),
    sum_sub(_, [second_product_div("-", product_div(_, number(1024.424), _))]),
    % '+' shouldn't matter
    sum_sub(_, [second_product_div("+", product_div(_, number(1), _))]),
    % second item in the list shouldn't matter
    sum_sub(_, [second_product_div("-", product_div(_, number(111), _)), 
                second_product_div("+", product_div(_, number(1), _))]).

test("sum_sub should fail if the sign is '-' and the first term of the second product div is < 100", [fail]) :-
    sum_sub(_, [second_product_div("-", product_div(_, number(1), _))]).


test("number to the rhs of product_div must != 0 if the sign is '/'") :-
    product_div(_, _, [second_term("/", number(1.24))]),
    product_div(_, _, [second_term("/", number(1346))]),
    % '*' shouldn't matter
    product_div(_, _, [second_term("*", number(63))]),
    % must hold for all items in the list
    product_div(_, _, [second_term("/", number(1346)), second_term("*", number(23)), second_term("/", number(634))]).

test("product_div must fail if number to the rhs of product_div is = 0 and the sign is '/'", [fail]) :-
    product_div(_, _, [second_term("/", number(1)), second_term("/", number(0))]).


test("All numbers generated must be > 5") :-
    term(452.124),
    term(7325),
    term(5.235),
    term(6).

test("Term must fail if the number generated is < 5", [fail]) :-
    term(-13).

test("Term must fail if the number generated is = 5", [fail]) :-
    term(5).

:- end_tests(arithexprules).