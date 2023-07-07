:- dynamic class/3.
:- dynamic field/4.
:- dynamic method/5.
:- dynamic formal_param/4.
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
