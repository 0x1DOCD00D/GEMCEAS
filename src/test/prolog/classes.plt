:- consult('../../main/prolog/classes.pl').

:- begin_tests(classes).

retract_class(ClassName) :-
    class(_, ClassName, _),
    retractall(class(_, ClassName, _)).

retract_fields_in_class(ClassName) :-
    field(_, _, _, ClassName),
    retractall(field(_, _, _, ClassName)).


test("Class modifiers") :-
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

test("Two same modifiers", [fail]) :-
    normal_class_declaration(["public", "public"], "SameMod", _).

test("Existing class", [setup(retract_class("Class1")), fail]) :-
    normal_class_declaration(["public"], "Class1", _),
    normal_class_declaration(["public"], "Class1", _).


test("Field modifiers") :-
    field_modifiers(F),
    F == ["public", "protected", "private", "static", "final", "transient", "volatile"].

test("Single field declaration", [cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x", "SomeClass").

test("Multiple field declaration", [cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x, y, z", "SomeClass").

test("Single field declaration with value", [cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x=1", "SomeClass").

test("Multiple field declaration with value", [cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x=1, y=2, z", "SomeClass").

test("Multiple queries for same identifier", [fail, cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x", "SomeClass"),
    field_declaration([], "int", "x=1", "SomeClass").

test("Single query with same identifier", [fail, cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x=1, x", "SomeClass").

test("Same identifier, different types", [fail, cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "double", "x", "SomeClass"),
    field_declaration([], "int", "x", "SomeClass").

test("Two same modifiers", [fail]) :-
    field_declaration(["private", "private"], "int", "x=1", "SomeClass").

test("Final volatile field", [fail]) :-
    field_declaration(["final", "volatile"], "int", "x=1", "SomeClass").

:- end_tests(classes).
