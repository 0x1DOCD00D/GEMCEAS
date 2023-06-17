% :- style_check(-singleton).

:- dynamic field/3.

% section 8
access_modifiers(['public', 'protected', 'private']).

overloaded(methods).
overloaded(constructors).



% section 8.1
class_declaration([normal_class_declaration, enum_declaration, record_declaration]).

class_modifier(C) :- 
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'sealed', 'non-sealed', 'strictfp'], C).


% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_class_declaration(ClassModifier, TypeIdentifier, EnclosingClassIdentifier) :- 
    is_list(ClassModifier),
    (class_modifier(C), sublist(ClassModifier, C)),
    % compile-time error if the same keyword appears more than once
    is_set(ClassModifier),
    % fail if class is static but doesn't have an immediate enclosing class
    ((member('static', ClassModifier), var(EnclosingClassIdentifier)) -> fail ; true),
    % fail if the class already exists
    (recorded(_, TypeIdentifier) ->
        false ; 
        ((member('abstract', ClassModifier); member('static', ClassModifier)) ->
            recordz(abstract_or_static_class, TypeIdentifier) ;
            recordz(normal_class, TypeIdentifier)
        )
    ).



% section 8.3
field_modifier(F) :-
    access_modifiers(A),
    append(A, ['static', 'final', 'transient', 'volatile'], F).

% FieldDeclaration:
%   {FieldModifier} UnannType VariableDeclaratorList
field_declaration(FieldModifier, VariableDeclaratorList, CurrentClass) :-
    is_list(FieldModifier),
    (field_modifier(F), sublist(FieldModifier, F)),
    % compile-time error if the same keyword appears more than once
    is_set(FieldModifier),
    % final variables cannot be volatile
    (member('final', FieldModifier) -> 
        (member('volatile', FieldModifier) -> false ; true) ;
        true),
    split_string(VariableDeclaratorList, ",", " ", VarList),
    assert_var_list(VarList, CurrentClass).
    

assert_var_list([], _).
assert_var_list([H|T], CurrentClass) :-
    (sub_string(H, _, _, _, "=") ->
        % split the var into name and value if the var has a value assigned
        (name_value(H, Name, Value), 
        (field(Name, _, CurrentClass) ->
            fail ;  % fail if the field already exists in the KB
            assertz(field(Name, Value, CurrentClass)))) ;
        % save the value as undefined otherwise
        (field(H, _, CurrentClass) ->
            fail ;
            assertz(field(H, undefined, CurrentClass)))),
    assert_var_list(T, CurrentClass).


name_value(String, Name, Value) :-
    sub_string(String, Before, _, After, "="),
    !,
    sub_string(String, 0, Before, _, Name),
    sub_string(String, _, After, 0, Value).


% section 8.4
method_modifier(M) :-
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'synchronized', 'native', 'strictfp'], M).


% MethodDeclaration:
%   {MethodModifier} MethodHeader MethodBody
method_declaration(MethodModifier, MethodBody) :-
    is_list(MethodModifier),
    (method_modifier(M), sublist(MethodModifier, M)),
    % compile-time error if the same keyword appears more than once
    is_set(MethodModifier),
    % cannot have more than one of the access modifiers public, protected, and private
    check_access_modifier(MethodModifier),
    % method cannot be both native and strictfp
    (member('native', MethodModifier), member('strictfp', MethodModifier) -> false ; true),
    % abstract methods cannot be private, static, final, native, strictfp, or synchronized,
    % which leaves three possible combinations abstract/public or abstract/protected or 
    % just abstract
    (member('abstract', MethodModifier) -> 
        (((member('public', MethodModifier); member('protected', MethodModifier));
         (length(MethodModifier, L), L == 1))) ; 
        true
    ),

    string_length(MethodBody, ML),
    % compile-time error if a method declaration is either abstract or native 
    % and has a block for its body
    (ML > 1 -> 
        (((member('abstract', MethodModifier); member('native', MethodModifier)) -> 
            false ; 
            true)) ; 
        true),
    % compile-time error if a method declaration is neither abstract nor native and 
    % has a semicolon for its body
    (ML == 1 ->
        (member('abstract', MethodModifier); member('native', MethodModifier)) ;
        true).
    

sublist([],_).
sublist([X|Xs],Y) :- member(X,Y) , sublist(Xs,Y).


% ensures that the modifier list has only one of the modifiers public, protected, or private
check_access_modifier(ModifierList) :-
    (member('public', ModifierList) -> 
        ((member('protected', ModifierList); member('private', ModifierList)) -> 
            false ; true) ;
        true
    ),
    (member('protected', ModifierList) -> 
        ((member('public', ModifierList); member('private', ModifierList)) -> 
            false ; true) ;
        true
    ),
    (member('private', ModifierList) -> 
        ((member('protected', ModifierList); member('public', ModifierList)) -> 
            false ; true) ;
        true
    ).



% section 8.8
% ConstructorDeclaration:
%   {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
constructor_declaration(ConstructorModifier) :-
    % TODO:
    % compile-time error to declare two constructors with override-equivalent signatures (§8.4.2) in a class.
    % compile-time error to declare two constructors whose signatures have the same erasure (§4.6) in a class.
    (access_modifiers(C), member(ConstructorModifier, C)).



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
