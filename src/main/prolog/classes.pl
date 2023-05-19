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

access_modifiers(A):- 
    A = ['public', 'protected', 'private'].

overloaded(methods).
overloaded(constructors).

% section 8.1
class_declaration :- normal_declaration(Cm, Ti) ; enum_declaration ; record_declaration.

class_modifier(C):- 
    access_modifiers(A),
    append(A, ['abstract', 'static', 'final', 'sealed', 'non-sealed', 'strictfp'], C).

% NormalClassDeclaration:
%   {ClassModifier} class TypeIdentifier [TypeParameters]
%   [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normal_declaration(ClassModifier, TypeIdentifier) :- 
    (class_modifier(C), member(ClassModifier, C)).
    string(TypeIdentifier).


% class(inner_class) :- member_class =\= static ; local_class =\= static ; anonymous_class.

% class(static) :- member_enum_class ;
%                  local_enum_class ;
%                  member_record_class ;
%                  local_record_class ;
%                  member_class_interface.



