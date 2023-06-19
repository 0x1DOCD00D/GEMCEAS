% section 15.9
class_or_interface_type_to_instantiate(Identifier, IdentifierList) :-
    % check whether the Identifier added to the KB by normal_class_declaration/3
    class(ClassModifier, Identifier, _),
    % recorded(X, Identifier),
    length(IdentifierList, IdentifierListLength),
    ((IdentifierListLength == 0) ->
      (\+ member('abstract', ClassModifier)) ;
      (nth1(IdentifierListLength, IdentifierList, LastIdentifier),
      class(LastIdentifierModifier, LastIdentifier, _),
      % ensure last identifier is not abstract
      \+ member('abstract', LastIdentifierModifier),
      % ensure last identifier is static
      member('static', LastIdentifierModifier),
      check_nested_class_order(IdentifierList)
      )
    ).

check_nested_class_order([]).
check_nested_class_order([Outer, Inner | T]) :-
  class(_, Inner, Outer),
  check_nested_class_order(T).
check_nested_class_order([Outer]) :-
  class(_, Outer, _).



dim_expr(Expression) :-
  evaluate(Expression, _).

evaluate(Expression, Result) :- 
  Result is Expression.



/*
TODO:
Chapter 15:
- It is a compile-time error if a this expression occurs in a static context (§8.1.3).
- It is a compile-time error if a qualified this expression occurs in a static context (§8.1.3).
- It is a compile-time error if the class or interface whose declaration immediately encloses a qualified this
  expression is not an inner class of TypeName or TypeName itself.
- Instantiating nested classes - rule ClassOrInterfaceTypeToInstantiate
- Array access expressions (15.10.3)
- It is a compile-time error if a field access expression using the keyword super appears in a static context (§8.1.3).
- For a field access expression of the form super.Identifier: It is a compile-time error if the immediately enclosing 
  class or interface declaration of the field access expression is the class Object or an interface. (section 15.11.2)
*/
