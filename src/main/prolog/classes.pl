%top_level_class.
%nested_class.
%member_class.
%local_class.
%anonymous_class.
%inner_class.
%enum_class.
%record_class.


nested_class :- member_class ; local_class ; anonymous_class.

class_body :- members ; instance_initializer ; static_initializer ; constructors.
members :- fields ; methods ; classes ; interfaces.

access_modifiers :- public ; protected ; private.

overloaded(methods).
overloaded(constructors).

class_declaration :- normal_declaration ; enum_declaration ; record_declaration.

class_modifier :- access_modifiers ; abstract ; static ; final ; sealed ; non_sealed ; strictfp.

class(inner_class) :- member_class =\= static ; local_class =\= static ; anonymous_class.

class(static) :- member_enum_class ;
                 local_enum_class ;
                 member_record_class ;
                 local_record_class ;
                 member_class_interface.



