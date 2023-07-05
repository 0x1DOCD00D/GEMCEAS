% section 15.9
class_or_interface_type_to_instantiate(Identifier, IdentifierList) :-
    % check whether the class was added to the KB by normal_class_declaration/3
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



method_invocation_simple(ClassIdentifier, EnclosingMethodModifiers, MethodName, ArgumentList) :-
    % (var(ArgumentList) ->
    %     ExpressionList=[] ;
    %     split_string(ArgumentList, ",", " ", ExpressionList)),
    % length(ExpressionList, ExpressionListLength),
    % findall(X, method(ClassIdentifier, _, MethodName, X, ExpressionListLength), ParamList),
    % length(ParamList, ParamListLength),
    % ParamListLength =\= 0,
    false.  % failing this rule till I figure out how to infer the type of an arg expression
    % TODO: check arg types
    % TODO: if enclosing method is static, invoked method should also be static



% section 15.28
switch_expression_types(['char', 'byte', 'short', 'int', 'Character', 'Byte', 'Short', 'Integer', 'String']).

switch_expression(Expression, SwitchBlock, ClassIdentifier) :-
    field(Expression, _, Value, ClassIdentifier),
    nonvar(Value),
    switch_expression_types(AllowedTypes),
    member(Type, AllowedTypes),
    check_default_block(SwitchBlock, 0).

switch_statement(Expression, SwitchBlock, ClassIdentifier) :-
    switch_expression(Expression, SwitchBlock, ClassIdentifier).


switch_block(SwitchLabel) :-
    % ensure the switch block only has one 'default' block
    check_default_block(SwitchLabel, 0).

check_default_block([], Occurrences) :-
    Occurrences < 2.
check_default_block([H|T], Occurrences) :-
    Occurrences >= 2 ->
        false ;
        (sub_string(H, _, _, _, "default") ->
            (OccurrencesPlusOne is Occurrences + 1,
            check_default_block(T, OccurrencesPlusOne)) ;
            check_default_block(T, Occurrences)
        ).


assignment(LeftHandSide, Expression) :-
    % TODO: validate more expressions
    % assuming both LHS and RHS are expression_name/2
    arg(1, LeftHandSide, LhsIdentifier),
    arg(2, LeftHandSide, LhsClassIdentifier),
    field(LhsIdentifier, LhsType, _, LhsClassIdentifier),
    arg(1, Expression, RhsIdentifier),
    arg(2, Expression, RhsClassIdentifier),
    field(RhsIdentifier, RhsType, _, RhsClassIdentifier),
    LhsType = RhsType.


expression_name(Identifier, ClassIdentifier) :-
    % TODO: validate ambiguous names
    field(Identifier, _, _, ClassIdentifier) ->
        true ;
        check_field_in_parent_class(Identifier, ClassIdentifier).

check_field_in_parent_class(ExpressionName, ClassIdentifier) :-
    class(_, ClassIdentifier, EnclosingClassIdentifier),
    (field(ExpressionName, _, _, EnclosingClassIdentifier) ->
        true ;
        (nonvar(EnclosingClassIdentifier), 
        check_field_in_parent_class(ExpressionName, EnclosingClassIdentifier))).



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
- Method Invocation, 15.12.3 Compile-Time Step 3: Is the Chosen Method Appropriate?
*/
