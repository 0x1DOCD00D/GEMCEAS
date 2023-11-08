:- consult('../../main/prolog/common.pl').

:- begin_tests(common).

retract_params :-
    formal_param(_, _),
    retractall(formal_param(_, _)).

test("Valid sublists") :-
    sublist([a], [a,b,c]),
    sublist([a,c], [a,b,c]),
    sublist([c,a], [a,b,c]),
    sublist([a,a], [a,b,c]),
    sublist([], [a,b,c]),
    sublist([[a],c], [a,b,c,[a]]), !.

test("Invalid sublist", [fail]) :-
    sublist([c], [a,b]).

test("Invalid nested sublist", [fail]) :-
    sublist([[a,c]], [[a,b],c]).


test("Valid access modifiers") :-
    check_access_modifier(["public"]),
    check_access_modifier(["private"]),
    check_access_modifier(["protected"]).

test("Public private access modifiers", [fail]) :-
    check_access_modifier(["public", "private"]).

test("Private public access modifiers", [fail]) :-
    check_access_modifier(["private", "public"]).

test("Public protected access modifiers", [fail]) :-
    check_access_modifier(["public", "protected"]).

test("Protected public access modifiers", [fail]) :-
    check_access_modifier(["protected", "public"]).

test("Private protected access modifiers", [fail]) :-
    check_access_modifier(["private", "protected"]).

test("Protected private access modifiers", [fail]) :-
    check_access_modifier(["protected", "private"]).

test("Name prologFact") :-
    name_value("x=1", Name, Value),
    Name="x", Value="1".

test("Overloaded param lists") :-
    check_overloaded_param_lists([formal_parameter("int", "x")], [[formal_parameter("double", "x")]]),
    check_overloaded_param_lists([formal_parameter("int", "x")], [[formal_parameter("double", "x")], 
        [formal_parameter("int", "x"), formal_parameter("int", "y")]]), !.

test("Overloaded param with same type", [fail]) :-
    check_overloaded_param_lists([formal_parameter("int", "x")], [[formal_parameter("int", "a")]]).

test("Multuple overloaded params with same type", [fail]) :-
    check_overloaded_param_lists([formal_parameter("int", "x"), formal_parameter("int", "y")], 
        [[formal_parameter("int", "a"), formal_parameter("int", "b")]]).


test("Formal params", [cleanup(retract_params)]) :-
    formal_parameter("int", "x"),
    formal_parameter("double", "abc").

test("'this' formal param identifier", [fail]) :-
    formal_parameter("int", "this").

test("Existing param identifier", [fail, cleanup(retract_params)]) :-
    formal_parameter("int", "a"),
    formal_parameter("double", "a").

test("formal_parameter_list/2 removes params added to KB added by formal_parameter/2") :-
    formal_parameter("int", "x"),
    formal_parameter("double", "abc"),
    formal_parameter_list(_, _),
    \+ formal_param("int", "x"),
    \+ formal_param("double", "abc").

:- end_tests(common).