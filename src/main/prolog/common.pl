:- dynamic class/3.
:- dynamic field/4.
:- dynamic method/5.
:- dynamic formal_param/2.
:- dynamic constructor/2.
:- dynamic interface/3.


% sublist(X,Y) checks if X is a sublist of Y
sublist([],_).
sublist([X|Xs],Y) :- member(X,Y) , sublist(Xs,Y).


% ensures that the modifier list has only one of the modifiers public, protected, private, and 
% one of sealed, non-sealed
check_access_modifier(ModifierList) :-
    (member("public", ModifierList) -> 
        ((member("protected", ModifierList); member("private", ModifierList)) -> 
            false ; true) ;
        true
    ),
    (member("protected", ModifierList) -> 
        ((member("public", ModifierList); member("private", ModifierList)) -> 
            false ; true) ;
        true
    ),
    (member("private", ModifierList) -> 
        ((member("protected", ModifierList); member("public", ModifierList)) -> 
            false ; true) ;
        true
    ),
    (member("sealed", ModifierList) ->
        \+ member("non-sealed", ModifierList) ;
        true
    ),
    (member("non-sealed", ModifierList) ->
        \+ member("sealed", ModifierList) ;
        true
    ).


% splits a variable declaration into its name and value
name_value(String, Name, Value) :-
    sub_string(String, Before, _, After, "="),
    !,
    sub_string(String, 0, Before, _, Name),
    sub_string(String, _, After, 0, Value).


% empty fact set to allow the generator to pass args up the parse tree
method_header(_).

method_declarator(_, _).


get_curr_param_list(FormalParameterList, CurrParamList) :-
    % check if the method / constructor has params
    (var(FormalParameterList) -> 
        CurrParamList=[] ; 
        (arg(1, FormalParameterList, ParamHead), arg(2, FormalParameterList, ParamTail),
        append([ParamHead], ParamTail, CurrParamList))
        ).


check_overloaded_param_lists(_, []).
check_overloaded_param_lists(CurrParamList, [PrevParamList | PrevParamListTail]) :-
    length(CurrParamList, CurrParamListLength),
    length(PrevParamList, PrevParamListLength),
    ((PrevParamListLength == CurrParamListLength) ->
        check_param_types(PrevParamList, CurrParamList) ;
        true),
    check_overloaded_param_lists(CurrParamList, PrevParamListTail).

check_param_types([PrevParam|PrevParamListTail], [CurrParam|CurrParamListTail]) :-
    arg(1, PrevParam, PrevType),
    arg(1, CurrParam, CurrType),
    ((\+ PrevType == CurrType) ->
        % param types are different. End check
        true ;
        % keep checking remaining params. Lack of a base case (empty param lists) will cause
        % the rule to fail eventually if all params match
        check_param_types(PrevParamListTail, CurrParamListTail)).


formal_parameter_list(_, _) :-
    % remove param details from the KB (to allow the same param name across overloaded methods / constructors).
    % Always returns true
    retractall(formal_param(_, _)).

formal_parameter(UnannType, VariableDeclaratorId) :-
    % "this" is reserved for the receiver param
    (\+ VariableDeclaratorId == "this"),
    % fail if there's another param with the same name
    \+ formal_param(_, VariableDeclaratorId),
    % add the param to the KB (facts removed later by method_declarator/2)
    assertz(formal_param(UnannType, VariableDeclaratorId)).
