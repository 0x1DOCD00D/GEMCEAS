:- ensure_loaded(common).

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


% section 9.4
interface_method_declaration(InterfaceIdentifier, InterfaceMethodModifier, MethodHeader, MethodBody) :-
    validate_interface_method_modifiers(InterfaceMethodModifier, MethodBody),
    arg(1, MethodHeader, MethodDeclarator),
    arg(1, MethodDeclarator, MethodIdentifier),
    arg(2, MethodDeclarator, FormalParameterList),
    % check if the method has params
    get_curr_param_list(FormalParameterList, CurrParamList),
    length(CurrParamList, CurrParamListLength),
    % check if any methods exist with the same name
    findall(X, method(InterfaceIdentifier, _, MethodIdentifier, X, CurrParamListLength), PrevParamList),
    check_overloaded_param_lists(CurrParamList, PrevParamList),
    % add method to KB
    assertz(method(InterfaceIdentifier, InterfaceMethodModifier, MethodIdentifier, CurrParamList, CurrParamListLength)).


validate_interface_method_modifiers(InterfaceMethodModifier, MethodBody) :-
    % compile-time error if the same keyword appears more than once as a modifier
    is_set(InterfaceMethodModifier),
    % cannot have more than one of the access modifiers public and private
    (member("public", InterfaceMethodModifier) -> 
        \+ member("private", InterfaceMethodModifier) ;
        true),
    (member("private", InterfaceMethodModifier) -> 
        \+ (member("public", InterfaceMethodModifier); 
            member("abstract", InterfaceMethodModifier); 
            member("default", InterfaceMethodModifier)) ;
        true),
    % compile-time error if an interface method declaration has more than one of the keywords 
    % abstract, default, or static
    (member("abstract", InterfaceMethodModifier) -> 
        \+ (member("default", InterfaceMethodModifier); 
            member("static", InterfaceMethodModifier);
            % compile-time error if an interface method declaration that contains the keyword abstract 
            % also contains the keyword strictfp
            member("strictfp", InterfaceMethodModifier)) ;
        true),
    (member("default", InterfaceMethodModifier) -> 
        \+ (member("abstract", InterfaceMethodModifier); member("static", InterfaceMethodModifier)) ;
        true),
    (member("static", InterfaceMethodModifier) -> 
        \+ (member("default", InterfaceMethodModifier); member("abstract", InterfaceMethodModifier)) ;
        true),
    string_length(MethodBody, BodyLength),
    ((member("private", InterfaceMethodModifier); 
      member("default", InterfaceMethodModifier); 
      member("static", InterfaceMethodModifier)) ->
        % private and default methods should have a method body
        BodyLength > 1 ;
        % other methods should only have a semicolon
        BodyLength == 1).
