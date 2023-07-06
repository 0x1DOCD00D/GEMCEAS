:- ensure_loaded(common).

:- dynamic interface/3.

% section 9.1
interface_modifiers(["public", "protected", "private", "abstract", "static", "sealed", "non-sealed", "strictfp"]).

normal_interface_declaration(InterfaceModifier, TypeIdentifier, EnclosingInterfaceIdentifier) :-
    (interface_modifiers(InterfaceModifierList), sublist(InterfaceModifier, InterfaceModifierList)),
    (var(EnclosingInterfaceIdentifier) ->
        % top level interfaces cannot be "protected", "private" or "static"
        (\+ member("protected", InterfaceModifier),
         \+ member("private", InterfaceModifier),
         \+ member("static", InterfaceModifier)) ;
        true),
    % compile-time error if the same keyword appears more than once
    is_set(InterfaceModifier),
    % cannot have more than one of the access modifiers public, protected, private, and
    % one of sealed, non-sealed
    check_access_modifier(InterfaceModifier),
    % fail if the interface exists in the KB
    \+ interface(InterfaceModifier, TypeIdentifier, EnclosingInterfaceIdentifier),
    % add interface details to KB
    assertz(interface(InterfaceModifier, TypeIdentifier, EnclosingInterfaceIdentifier)).


% section 9.3
constant_modifiers(["public", "static", "final"]).

constant_declaration(ConstantModifier, UnannType, VariableDeclaratorList, CurrentInterface) :-
    (constant_modifiers(ConstantModifierList), sublist(ConstantModifier, ConstantModifierList)),
    % compile-time error if the same keyword appears more than once
    is_set(ConstantModifier),
    % every declarator in a field declaration of an interface must have a variable initializer
    split_string(VariableDeclaratorList, ",", " ", VarList),
    assert_const_list(VarList, UnannType, CurrentInterface).


assert_const_list([], _, _).
assert_const_list([H|T], UnannType, CurrentInterface) :-
    % check if the var is initialized
    sub_string(H, _, _, _, "="),
    % split the var into name and value if the var has a value assigned
    name_value(H, Name, Value),
    % fail if the field already exists in the KB
    \+ field(Name, _, _, CurrentInterface),
    assertz(field(Name, UnannType, Value, CurrentInterface)),
    assert_const_list(T, UnannType, CurrentInterface).
