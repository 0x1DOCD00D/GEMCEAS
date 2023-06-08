% section 15.9
class_or_interface_type_to_instantiate(Identifier) :-
    % check whether the Identifier was recorded by normal_class_declaration/3
    recorded(class_name, Identifier).


/*
TODO:
Chapter 15:
- It is a compile-time error if a this expression occurs in a static context (§8.1.3).
- It is a compile-time error if a qualified this expression occurs in a static context (§8.1.3).
- It is a compile-time error if the class or interface whose declaration immediately encloses a qualified this
  expression is not an inner class of TypeName or TypeName itself.
- Instantiating nested classes - rule ClassOrInterfaceTypeToInstantiate
*/
