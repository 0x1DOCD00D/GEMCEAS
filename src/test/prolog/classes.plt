:- consult('../../main/prolog/classes.pl').

:- begin_tests(classes).

retract_class(ClassName) :-
    retractall(classes(_, ClassName, _)).

test(class_modifiers) :-
    class_modifiers(C),
    C == ["public", "protected", "private", "abstract", "static", "final", "sealed", "non-sealed", "strictfp"].

test("Simple top-level class", [nondet]) :-
    normal_class_declaration(["public"], "Class1", _).

test("Inner Class", [nondet]) :-
    normal_class_declaration([], "OuterClass1", _),
    normal_class_declaration(["public"], "InnerClass", "OuterClass1").

test("Static Class", [nondet]) :-
    normal_class_declaration([], "OuterClass2", _),
    normal_class_declaration(["static"], "StaticClass", "OuterClass1").

test("Same class name with different modifiers", [fail]) :-
    normal_class_declaration(["public"], "Class2", _),
    normal_class_declaration([], "Class2", _).

test("Private top-level class", [fail]) :-
    normal_class_declaration(["private"], "PrivateClass", _).

test("Static top-level class", [fail]) :-
    normal_class_declaration(["static"], "StaticClass", _).

test("Protected top-level class", [fail]) :-
    normal_class_declaration(["protected"], "ProtectedClass", _).

test("Public and private top-level class", [fail]) :-
    normal_class_declaration(["public", "private"], "PublicPrivateClass", _).

test("Public and protected top-level class", [fail]) :-
    normal_class_declaration(["public", "protected"], "PublicProtectedClass", _).

test("Private and protected top-level class", [fail]) :-
    normal_class_declaration(["private", "protected"], "PrivateProtectedClass", _).

test("Existing class", [fail]) :-
    normal_class_declaration(["public"], "Class1", _),
    normal_class_declaration(["public"], "Class1", _).

:- end_tests(classes).
