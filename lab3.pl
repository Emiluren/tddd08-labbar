[scanner]. % for scan
[lab2]. % for execute

% run(In, String, Out)
% Out is the resulting environment from running the program represented by the string String
% in the starting environment In
run(In, String, Out) :-
    scan(String, Tokens),
    parse(Tokens, AbstStx),
    execute(AbstStx, In, Out).

% parse(Tokens, AbstStx)
% AbstStx is the abstract syntax tree for the token list Tokens
parse(Tokens, AbstStx) :- parse_cmd(Tokens, AbstStx).
parse(Tokens, seq(AbstStxCmd, AbstStxPgm)) :-
    append(Cmd, [; | Pgm], Tokens),
    parse_cmd(Cmd, AbstStxCmd),
    parse(Pgm, AbstStxPgm).

% concat(Xs, Ys)
% Xs is a list of lists and Ys is all those lists appended in order
concat([], []).
concat([X|Xs], Y) :- append(X, XTail, Y), concat(Xs, XTail).

% parse_cmd(Tokens, AbstStx)
% AbstStx is the abstract syntax tree for the command represented by the token
% list Tokens
parse_cmd([skip], skip).
parse_cmd([Id, := | Expr], set(Id, ExprAbstStx)) :-
    parse_expr(Expr, ExprAbstStx).
parse_cmd(Tokens, if(BoolAbstStx, PgmThenAbstStx, PgmElseAbstStx)) :-
    concat([[if], Bool, [then], PgmThen, [else], PgmElse, [fi]], Tokens),
    parse_bool(Bool, BoolAbstStx),
    parse(PgmThen, PgmThenAbstStx),
    parse(PgmElse, PgmElseAbstStx).
parse_cmd(Tokens, while(BoolAbstStx, PgmAbstStx)) :-
    concat([[while], Bool, [do], Pgm, [od]], Tokens),
    parse_bool(Bool, BoolAbstStx),
    parse(Pgm, PgmAbstStx).

% parse_bool(Tokens, AbstStx)
% AbstStx is the abstract syntax tree for the boolean expression represented by
% the token list Tokens
parse_bool(Tokens, EStx1 > EStx2) :-
    concat([E1, [>], E2], Tokens),
    parse_expr(E1, EStx1),
    parse_expr(E2, EStx2).

parse_expr(Tokens, FStx * EStx) :-
    concat([F, [*], E], Tokens),
    parse_factor(F, FStx),
    parse_expr(E, EStx).
parse_expr(Tokens, FStx) :-
    parse_factor(Tokens, FStx).

parse_factor(Tokens, TStx + FStx) :-
    concat([T, [+], F], Tokens),
    parse_term(T, TStx),
    parse_factor(F, FStx).
parse_factor(Tokens, TStx) :-
    parse_term(Tokens, TStx).

parse_term([id(X)], id(X)).
parse_term([num(X)], num(X)).
