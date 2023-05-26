:- ensure_loaded(sectionlinks).
% :- style_check(-singleton).

% section 8
access_modifiers(['public', 'protected', 'private']).

overloaded(methods).
overloaded(constructors).

% section 8.1
class_declaration([normal_class_declaration, enum_declaration, record_declaration]).

class_modifier(C) :- 
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'sealed', 'non-sealed', 'strictfp'], C).

% 'member' and 'inner' are both nested classes but inner classes cannot be static
class_type(['top-level', 'member', 'inner']).

% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_class_declaration(ClassModifier, TypeIdentifier, ClassType) :- 
    (class_modifier(C), member(ClassModifier, C)),
    (class_type(T), member(ClassType, T)),
    (ClassModifier == static -> ClassType == 'member'; true),
    string(TypeIdentifier).
    % TODO: implement checks for 'extends', 'implements', 'permits', 'classbody'

% section 8.3
field_modifier(F) :-
    access_modifiers(A),
    append(A, ['static', 'final', 'transient', 'volatile'], F).

% FieldDeclaration:
%   {FieldModifier} UnannType VariableDeclaratorList
field_declaration(FieldModifier, VariableDeclaratorList) :-
    (field_modifier(F), member(FieldModifier, F)),
    (is_list(VariableDeclaratorList)).
    % TODO: validate UnannType (UnannType: UnannPrimitiveType UnannReferenceType)
    % TODO: final variables cannot be volatile


% TODO: fields and classes can have multiple modifiers


% section 8.4
method_modifier(M) :-
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'synchronized', 'native', 'strictfp'], M).

method_body(['block', 'semicolon']).

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

    (method_body(B), member(MethodBody, B)),
    % compile-time error if a method declaration is either abstract or native 
    % and has a block for its body
    (MethodBody == 'block' -> 
        (((member('abstract', MethodModifier); member('native', MethodModifier)) -> 
            false ; 
            true)) ; 
        true),
    % compile-time error if a method declaration is neither abstract nor native and 
    % has a semicolon for its body
    (MethodBody == 'semicolon' ->
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



/* 
TODO: sections to come back to:
8.1.1.1 abstract Classes
8.1.1.2 sealed, non-sealed, and final Classes
8.1.4 Superclasses and Subclasses
8.1.5 Superinterfaces
8.1.6 Permitted Direct Subclasses
8.4.5 Method Result
8.4.7 Method Body (return statements)
*/
