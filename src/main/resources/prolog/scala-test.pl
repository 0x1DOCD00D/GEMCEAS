generate_integers.

% ensure both operands are integers and the RHS is not 0
div(Lhs, Rhs) :-
    integer(Lhs),
    integer(Rhs),
    Rhs =\= 0.

% ensure both operands are integers and one of them is negative
mul(Lhs, Rhs) :-
    integer(Lhs),
    integer(Rhs),
    (Lhs >= 0, Rhs < 0); (Lhs < 0, Rhs >= 0).

% ensure both operands are integers and addition is greater than INT_MIN and does not exceed INT_MAX
add(Lhs, Rhs) :-
    integer(Lhs),
    integer(Rhs),
    (Lhs + Rhs) >= -2147483648,
    (Lhs + Rhs) =< 2147483647.
