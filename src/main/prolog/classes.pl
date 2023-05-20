:- ensure_loaded(sectionlinks).

%top_level_class.
%nested_class.
% member_class.
% local_class.
% anonymous_class.

% instance_initializer.
% static_initializer.
% constructors.
% fields.
% methods.
% classes.
% interfaces.
%inner_class.
%enum_class.
%record_class.


% section 8
% nested_class :- member_class ; local_class ; anonymous_class.

% class_body :- members ; instance_initializer ; static_initializer ; constructors.
% members :- fields ; methods ; classes ; interfaces.

access_modifiers(A) :- 
    A = ['public', 'protected', 'private'].

overloaded(methods).
overloaded(constructors).

% section 8.1
class_declaration :- 
    normal_declaration(Cm, Ti, Ec, Ii, Pc, Cb);
    enum_declaration;
    record_declaration.

class_modifier(C) :- 
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'sealed', 'non-sealed', 'strictfp'], C).

% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_declaration(ClassModifier, TypeIdentifier, ExtendedClasses, ImplementedInterfaces, PermittedClasses, ClassBody) :- 
    (class_modifier(C), member(ClassModifier, C)),
    string(TypeIdentifier).
    % TODO: implement checks for 'extends', 'implements', 'permits', 'classbody'



/* 
Sections to come back to:
8.1.1.1 abstract Classes
8.1.1.2 sealed, non-sealed, and final Classes
8.1.2 Generic Classes and Type Parameters
8.1.4 Superclasses and Subclasses
8.1.5 Superinterfaces
8.1.6 Permitted Direct Subclasses
*/
