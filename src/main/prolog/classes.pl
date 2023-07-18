% :- style_check(-singleton).

:- ensure_loaded(common).

% section 8
access_modifiers(["public", "protected", "private"]).


% section 8.1
class_declaration([normal_class_declaration, enum_declaration, record_declaration]).

class_modifiers(C) :- 
    access_modifiers(A),
    append(A, ["abstract", "static", "final", "sealed", "non-sealed", "strictfp"], C).


% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_class_declaration(ClassModifier, TypeIdentifier, EnclosingClassIdentifier) :- 
    is_list(ClassModifier),
    (class_modifiers(C), sublist(ClassModifier, C)),
    % compile-time error if the same keyword appears more than once
    is_set(ClassModifier),
    % fail if class is static but doesn't have an immediate enclosing class
    % ((member("static", ClassModifier), var(EnclosingClassIdentifier)) -> fail ; true),
    (var(EnclosingClassIdentifier) ->
        \+(member("static", ClassModifier); 
            member("private", ClassModifier);
            member("protected", ClassModifier)) ;
        true),
    (nonvar(EnclosingClassIdentifier) ->
        % ensure inner class doesn't have the same name as the enclosing class
        ((\+ TypeIdentifier == EnclosingClassIdentifier),
        % ensure enclosing class exists
        class(_, EnclosingClassIdentifier, _)) ;
        true),
    % fail if the class already exists, else add class details to the KB
    (class(_, TypeIdentifier, EnclosingClassIdentifier) -> 
        fail ;
        assertz(class(ClassModifier, TypeIdentifier, EnclosingClassIdentifier))).


% section 8.3
field_modifiers(F) :-
    access_modifiers(A),
    append(A, ["static", "final", "transient", "volatile"], F).

% FieldDeclaration:
%   {FieldModifier} UnannType VariableDeclaratorList
field_declaration(FieldModifier, UnannType, VariableDeclaratorList, CurrentClass) :-
    is_list(FieldModifier),
    (field_modifiers(F), sublist(FieldModifier, F)),
    % compile-time error if the same keyword appears more than once
    is_set(FieldModifier),
    % final variables cannot be volatile
    (member("final", FieldModifier) -> 
        (member("volatile", FieldModifier) -> false ; true) ;
        true),
    split_string(VariableDeclaratorList, ",", " ", VarList),
    assert_var_list(VarList, UnannType, CurrentClass).
    

assert_var_list([], _, _).
assert_var_list([H|T], UnannType, CurrentClass) :-
    (sub_string(H, _, _, _, "=") ->
        % split the var into name and value if the var has a value assigned
        (name_value(H, Name, Value), 
        (field(Name, _, _, CurrentClass) ->
            fail ;  % fail if the field already exists in the KB
            assertz(field(Name, UnannType, Value, CurrentClass)))) ;
        % save the value as undefined otherwise
        (field(H, _, _, CurrentClass) ->
            fail ;
            assertz(field(H, UnannType, _, CurrentClass)))),
    assert_var_list(T, UnannType, CurrentClass).



% section 8.4
method_modifiers(M) :-
    access_modifiers(A),
    append(A, ["abstract", "static", "final", "synchronized", "native", "strictfp"], M).


% MethodDeclaration:
%   {MethodModifier} MethodHeader MethodBody
method_declaration(ClassIdentifier, MethodModifier, MethodHeader, MethodBody) :-
    validate_class_method_modifiers(MethodModifier, MethodBody),
    arg(1, MethodHeader, MethodDeclarator),
    arg(1, MethodDeclarator, MethodIdentifier),
    arg(2, MethodDeclarator, FormalParameterList),
    get_curr_param_list(FormalParameterList, CurrParamList),
    length(CurrParamList, CurrParamListLength),
    % check if any methods exist with the same name
    findall(X, method(ClassIdentifier, _, MethodIdentifier, X, CurrParamListLength), PrevParamLists),
    check_overloaded_param_lists(CurrParamList, PrevParamLists),
    % add method to KB
    assertz(method(ClassIdentifier, MethodModifier, MethodIdentifier, CurrParamList, CurrParamListLength)).


validate_class_method_modifiers(MethodModifier, MethodBody) :-
    is_list(MethodModifier),
    (method_modifiers(M), sublist(MethodModifier, M)),
    % compile-time error if the same keyword appears more than once
    is_set(MethodModifier),
    % cannot have more than one of the access modifiers public, protected, private, and
    % one of sealed, non-sealed
    check_access_modifier(MethodModifier),
    % method cannot be both native and strictfp
    (member("native", MethodModifier), member("strictfp", MethodModifier) -> false ; true),
    % abstract methods cannot be private, static, final, native, strictfp, or synchronized,
    % which leaves three possible combinations abstract/public or abstract/protected or 
    % just abstract
    (member("abstract", MethodModifier) -> 
        (((member("public", MethodModifier); member("protected", MethodModifier));
         (length(MethodModifier, L), L == 1))) ; 
        true
    ),
    string_length(MethodBody, ML),
    % compile-time error if a method declaration is either abstract or native 
    % and has a block for its body
    (ML > 1 -> 
        (((member("abstract", MethodModifier); member("native", MethodModifier)) -> 
            false ; 
            true)) ; 
        true),
    % compile-time error if a method declaration is neither abstract nor native and 
    % has a semicolon for its body
    (ML == 1 ->
        (member("abstract", MethodModifier); member("native", MethodModifier)) ;
        true).

get_curr_param_list(FormalParameterList, CurrParamList) :-
    % check if the method / constructor has params
    (var(FormalParameterList) -> 
        CurrParamList=[] ; 
        (arg(1, FormalParameterList, ParamHead), arg(2, FormalParameterList, ParamTail),
        append([ParamHead], ParamTail, CurrParamList))
        ).



% section 8.8
% ConstructorDeclaration:
%   {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
constructor_declaration(ConstructorModifier, ConstructorDeclarator) :-
    (access_modifiers(C), sublist(ConstructorModifier, C)),
    is_set(ConstructorModifier),
    arg(1, ConstructorDeclarator, SimpleTypeName),
    arg(2, ConstructorDeclarator, FormalParameterList),
    get_curr_param_list(FormalParameterList, CurrParamList),
    % check if any constructors exist with the same name
    findall(X, constructor(SimpleTypeName, X), PrevParamLists),
    check_overloaded_param_lists(CurrParamList, PrevParamLists),
    % add constructor details to KB
    assertz(constructor(SimpleTypeName, CurrParamList)).


constructor_declarator(_, _).



/*
Covered sections:
8.1 Class Declarations
8.2 Class Members
8.3 Field Declarations
8.4 Method Declarations
8.8 Constructor Declarations
*/

/* 
TODO: sub sections to come back to:
8.1.1.1 abstract Classes
8.1.1.2 sealed, non-sealed, and final Classes
8.1.4 Superclasses and Subclasses
8.1.5 Superinterfaces
8.1.6 Permitted Direct Subclasses
8.4.5 Method Result
8.4.7 Method Body (return statements)
8.4.8 Inheritance, Overriding, and Hiding
*/
