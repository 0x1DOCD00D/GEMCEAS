:- consult('../../main/resources/prolog/arithexprules.pl').

:- begin_tests(arithexprules).


test("if sum_sub's sign is '-' the first term of the product div should be > 100") :-
    sum_sub(_, [product_div_repetition("-", product_div(_, term(120), _))]),
    sum_sub(_, [product_div_repetition("-", product_div(_, term(1024.424), _))]),
    sum_sub(_, [product_div_repetition("-", product_div(_, term(1603), [term_repetition("/", term(6.23))]))]),
    sum_sub(_, [product_div_repetition("-", product_div(_, term(_, (sum_sub(_, 
        [product_div_repetition("-", product_div(_, term(363), _))])), _), [term_repetition("/", term(745))]))]),
    % '+' shouldn't matter
    sum_sub(_, [product_div_repetition("+", product_div(_, term(1), _))]),
    % second item in the list shouldn't matter
    sum_sub(_, [product_div_repetition("-", product_div(_, term(111), _)), 
                product_div_repetition("+", product_div(_, term(1), _))]).

test("sum_sub should fail if the sign is '-' and the first term of the product div is < 100", [fail]) :-
    sum_sub(_, [product_div_repetition("-", product_div(_, term(1), _))]).


test("number to the rhs of product_div must != 0 if the sign is '/'") :-
    product_div(_, _, [term_repetition("/", term(1.24))]),
    product_div(_, _, [term_repetition("/", term(1346))]),
    product_div(_, _, [term_repetition("/", term(_, (sum_sub(_, [product_div_repetition("-", product_div(_, 
        term(638), _))])), _))]),
    product_div(_, _, [term_repetition("/", term(_, (sum_sub(_, [product_div_repetition("-", product_div(_, 
        term(_, (sum_sub(_, [product_div_repetition("-", product_div(_, term(120), _))])), _), _))])), _))]),
    % '*' shouldn't matter
    product_div(_, _, [term_repetition("*", term(63))]),
    % must hold for all items in the list
    product_div(_, _, [term_repetition("/", term(1346)), term_repetition("*", term(23)), term_repetition("/", 
        term(634))]).

test("product_div must fail if number to the rhs of product_div is = 0 and the sign is '/'", [fail]) :-
    product_div(_, _, [term_repetition("/", term(1)), term_repetition("/", term(0))]).


test("All numbers generated must be > -5") :-
    term(452.124),
    term(7325),
    term(5.235),
    term(0),
    term(6).

test("Term must fail if the number generated is < -5", [fail]) :-
    term(-13).

test("Term must fail if the number generated is = -5", [fail]) :-
    term(-5).

:- end_tests(arithexprules).
