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
