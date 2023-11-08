:- consult('../../main/prolog/classes.pl').

:- begin_tests(classes).

retract_class(ClassName) :-
    class(_, ClassName, _),
    retractall(class(_, ClassName, _)).

retract_fields_in_class(ClassName) :-
    field(_, _, _, ClassName),
    retractall(field(_, _, _, ClassName)).

retract_methods_in_class(ClassName) :-
    method(ClassName, _, _, _, _),
    retractall(method(ClassName, _, _, _, _)).

retract_constructors_in_class(ClassName) :-
    constructor(ClassName, _),
    retractall(constructor(ClassName, _)).


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

test("Single field declaration with prologFact", [cleanup(retract_fields_in_class("SomeClass"))]) :-
    field_declaration([], "int", "x=1", "SomeClass").

test("Multiple field declaration with prologFact", [cleanup(retract_fields_in_class("SomeClass"))]) :-
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


test("Method modifiers") :-
    method_modifiers(M),
    M == ["public", "protected", "private", "abstract", "static", "final", "synchronized", "native", "strictfp"].

test("Simple method declaration", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", _)), "{}").

test("Two same modifiers", [fail]) :-
    method_declaration("Class1", ["public", "public"], method_header(method_declarator("method1", _)), "{}").

test("Sealed and non-sealed method", [fail]) :-
    method_declaration("Class1", ["sealed", "non-sealed"], method_header(method_declarator("method1", _)), "{}").

test("Native and strictfp", [fail]) :-
    method_declaration("Class1", ["native", "strictfp"], method_header(method_declarator("method1", _)), "{}").

test("Private abstract method ", [fail]) :-
    method_declaration("Class1", ["private", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Static abstract method ", [fail]) :-
    method_declaration("Class1", ["static", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Final abstract method ", [fail]) :-
    method_declaration("Class1", ["final", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Native abstract method ", [fail]) :-
    method_declaration("Class1", ["native", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Strictfp abstract method ", [fail]) :-
    method_declaration("Class1", ["strictfp", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Public abstract method ", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["public", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Protected abstract method ", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["protected", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Simple Abstract method", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["abstract"], method_header(method_declarator("method1", _)), ";").

test("Abstract method with body", [fail]) :-
    method_declaration("Class1", ["abstract"], method_header(method_declarator("method1", _)), "{}").

test("Native method with body", [fail]) :-
    method_declaration("Class1", ["native"], method_header(method_declarator("method1", _)), "{}").

test("Non abstract / native method without body", [fail]) :-
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", _)), ";").

test("Method with params", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), [formal_parameter("int", "y"), 
            formal_parameter("int", "z")]))), "{}").

test("Overloaded method", [nondet, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), []))), "{}"),
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), [formal_parameter("int", "y")]))), "{}"),
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), [formal_parameter("int", "y"), 
            formal_parameter("int", "z")]))), "{}").

test("Same method", [fail, cleanup(retract_methods_in_class("Class1"))]) :-
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), []))), "{}"),
    method_declaration("Class1", ["public"], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), []))), "{}").


test("Simple constructor declaration", [nondet, cleanup(retract_constructors_in_class("C1"))]) :-
    constructor_declaration(["public"], constructor_declarator("C1", _)).

test("Constructor with multiple modifiers", [fail]) :-
    constructor_declaration(["public", "private"], constructor_declarator("C1", _)).

test("Constructor with same modifiers", [fail]) :-
    constructor_declaration(["public", "public"], constructor_declarator("C1", _)).

test("Constructor with single param", [nondet, cleanup(retract_constructors_in_class("C1"))]) :-
    constructor_declaration(["public"], constructor_declarator("C1", 
        formal_parameter_list(formal_parameter("int", "x"), []))).

test("Constructor with multiple params", [nondet, cleanup(retract_constructors_in_class("C1"))]) :-
    constructor_declaration(["public"], constructor_declarator("C1", 
        formal_parameter_list(formal_parameter("int", "x"), [formal_parameter("int", "y"), 
            formal_parameter("int", "z")]))).

test("Same constructor", [fail, cleanup(retract_constructors_in_class("C1"))]) :-
    constructor_declaration(["public"], constructor_declarator("C1", _)),
    constructor_declaration(["public"], constructor_declarator("C1", _)).


:- end_tests(classes).
