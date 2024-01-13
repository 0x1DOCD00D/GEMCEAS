:- consult('../../main/resources/prolog/arithexpruleswithmetavars.pl').

:- begin_tests(arithexpruleswithmetavars).


test("Metavar in product_div/4") :-
    product_div(_, _, _, [term_repetition("/", term(1.24))]),
    product_div(_, _, _, [term_repetition("/", term(_, (sum_sub(_, 
        [product_div_repetition("-", product_div(_, _, term(638), _))])), _))]),
    product_div(product_div(_, _, term(_, 23), [term_repetition("*", term(63))]), _, _, 
        [term_repetition("/", term(1.24))]).

test("Valid metavar in term/2") :-
    term(_, 7325),
    term(_, 5.235),
    term(_, 0),
    term([term(_, 245), term(_, 64)], 553),
    term([term(_, 0)], 98).

test("Inalid metavar in term/2", [fail]) :-
    % metavar must be a list
    term(term(_, 245), 553).
