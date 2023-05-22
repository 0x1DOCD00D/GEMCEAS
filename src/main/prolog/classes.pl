:- ensure_loaded(sectionlinks).
% :- style_check(-singleton).

% section 8
access_modifiers(['public', 'protected', 'private']).

overloaded(methods).
overloaded(constructors).

% section 8.1
class_declaration([normal_declaration, enum_declaration, record_declaration]).

class_modifier(C) :- 
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'sealed', 'non-sealed', 'strictfp'], C).

% 'member' and 'inner' are both nested classes but inner classes cannot be static
class_type(['top-level', 'member', 'inner']).

% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_declaration(ClassModifier, TypeIdentifier, ClassType) :- 
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



/* 
Sections to come back to:
8.1.1.1 abstract Classes
8.1.1.2 sealed, non-sealed, and final Classes
8.1.2 Generic Classes and Type Parameters
8.1.4 Superclasses and Subclasses
8.1.5 Superinterfaces
8.1.6 Permitted Direct Subclasses
*/
