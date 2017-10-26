
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
parse(Tokens, AbstStx) :- program(AbstStx, Tokens, []).

program(Cmd) --> command(Cmd).
program(seq(Cmd, Pgm)) --> command(Cmd), [;], program(Pgm).

command(skip) --> [skip].
command(set(id(I), Exp)) --> [id(I), :=], expr(Exp).
command(if(Bool, PgmThen, PgmElse)) -->
    [if], bool(Bool), [then], program(PgmThen), [else], program(PgmElse), [fi].
command(while(Bool, Pgm)) --> [while], bool(Bool), [do], program(Pgm), [od].

bool(Exp1 > Exp2) --> expr(Exp1), [>], expr(Exp2).

expr(Factor * Expr) --> factor(Factor), [*], expr(Expr).
expr(Factor) --> factor(Factor).

factor(Term + Factor) --> term(Term), [+], factor(Factor).
factor(Factor) --> term(Factor).

term(id(X)) --> [id(X)].
term(num(X)) --> [num(X)].
