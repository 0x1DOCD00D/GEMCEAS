:- consult('../../main/prolog/interfaces.pl').

:- begin_tests(interfaces).

retract_interface(InterfaceName) :-
    interface(_, InterfaceName, _),
    retractall(interface(_, InterfaceName, _)).

retract_constant_in_interface(InterfaceName) :-
    field(_, _, _, InterfaceName),
    retractall(field(_, _, _, InterfaceName)).

retract_method_in_interface(InterfaceName) :-
    method(InterfaceName, _, _, _, _),
    retractall(method(InterfaceName, _, _, _, _)).


test("Simple interface declaration", [nondet, cleanup(retract_interface("Interface1"))]) :-
    normal_interface_declaration(["public"], "Interface1", _).

test("Nested interface", [cleanup(retract_interface("InnerInterface"))]) :-
    normal_interface_declaration([], "InnerInterface", "OuterInterface").

test("Private top-level interface", [fail]) :-
    normal_interface_declaration(["private"], "Interface1", _).

test("Protected top-level interface", [fail]) :-
    normal_interface_declaration(["protected"], "Interface1", _).

test("Static top-level interface", [fail]) :-
    normal_interface_declaration(["static"], "Interface1", _).

test("Same modifiers", [fail]) :-
    normal_interface_declaration(["public", "public"], "Interface1", _).

test("Public and private interface", [fail]) :-
    normal_interface_declaration(["public", "private"], "Interface1", _).

test("Public and protected interface", [fail]) :-
    normal_interface_declaration(["public", "protected"], "Interface1", _).

test("Private and protected interface", [fail]) :-
    normal_interface_declaration(["private", "protected"], "Interface1", _).

test("Existing interface", [fail, cleanup(retract_interface("Interface1"))]) :-
    normal_interface_declaration(["public"], "Interface1", _),
    normal_interface_declaration(["public"], "Interface1", _).


test("Interface constant declaration", [nondet, cleanup(retract_constant_in_interface("Interface1"))]) :-
    constant_declaration([], "int", "x=1", "Interface1").

test("Multiple constants", [nondet, cleanup(retract_constant_in_interface("Interface1"))]) :-
    constant_declaration([], "int", "x=1, y=2, z=3", "Interface1").

test("Same constants", [fail, cleanup(retract_constant_in_interface("Interface1"))]) :-
    constant_declaration([], "int", "x=1, x=2", "Interface1").

test("Interface constant modifiers", [nondet, cleanup(retract_constant_in_interface("Interface1"))]) :-
    constant_declaration(["public", "static", "final"], "int", "x=1", "Interface1").

test("Same modifiers", [fail]) :-
    constant_declaration(["public", "public"], "int", "x=1", "Interface1").

test("Uninitialized constant", [fail]) :-
    constant_declaration([], "int", "x", "Interface1").


test("Simple interface method declaration", [nondet, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", _)), ";").

test("Static interface method", [nondet, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", ["static"], method_header(method_declarator("method1", _)), "{}").

test("Method with single param", [nondet, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), []))), ";").

test("Method with multiple params", [nondet, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), [formal_parameter("int", "y"), 
            formal_parameter("int", "x")]))), ";").

test("Method with two same modifiers", [fail]) :-
    interface_method_declaration("I1", ["public", "public"], method_header(method_declarator("method1", _)), ";").

test("Public and private method", [fail]) :-
    interface_method_declaration("I1", ["public", "private"], method_header(method_declarator("method1", _)), ";").

test("Private abstract method", [fail]) :-
    interface_method_declaration("I1", ["private", "abstract"], method_header(method_declarator("method1", _)), ";").

test("Private default method", [fail]) :-
    interface_method_declaration("I1", ["private", "default"], method_header(method_declarator("method1", _)), ";").

test("Abstract default static method", [fail]) :-
    interface_method_declaration("I1", ["abstract", "default", "static"], 
        method_header(method_declarator("method1", _)), ";").

test("Abstract strictfp method", [fail]) :-
    interface_method_declaration("I1", ["abstract", "strictfp"], method_header(method_declarator("method1", _)), ";").

test("Private method without body", [fail]) :-
    interface_method_declaration("Interface1", ["private"], method_header(method_declarator("method1", _)), ";").

test("Default method without body", [fail]) :-
    interface_method_declaration("Interface1", ["default"], method_header(method_declarator("method1", _)), ";").

test("Static method without body", [fail]) :-
    interface_method_declaration("Interface1", ["static"], method_header(method_declarator("method1", _)), ";").

test("Existing method", [fail, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", _)), ";"),
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", _)), ";").

test("Overloaded methods", [nondet, cleanup(retract_method_in_interface("Interface1"))]) :-
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", _)), ";"),
    interface_method_declaration("Interface1", [], method_header(method_declarator("method1", 
        formal_parameter_list(formal_parameter("int", "x"), []))), ";").


:- end_tests(interfaces).
