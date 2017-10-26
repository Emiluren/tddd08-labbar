% 2.1
% isort(Xs, Ys)
% Ys is a insert sorted version of Xs
isort([], []).
isort([X|Xs], Ys) :-
    isort(Xs, Xsorted),
    insert(Xsorted, X, Ys).

% insert(Xs, Y, Ys)
% Ys is Xs with Y inserted at the position where all elements before Y are < Y
insert([], Y, [Y]).
insert([X|Xs], Y, [Y,X|Xs]) :- Y =< X.
insert([X|Xs], Y, [X|Ys]) :- Y > X, insert(Xs, Y, Ys).

% qsort(Xs, Ys)
% Ys is a quick sorted version of Ys
qsort([], []).
qsort([X|Xs], Ys) :-
    partition(Xs, X, Less, Greater),
    qsort(Less, Ls),
    qsort(Greater, Gs),
    append(Ls, [X|Gs], Ys).

% partition(Xs, Y, Less, Greater)
% Less is a list with the elements from Xs that are < Y and
% Greater is a list with the elements from Xs that are >= Y and
partition([], _, [], []).
partition([X|Xs], Y, [X|Less], Greater) :-
    X < Y,
    partition(Xs, Y, Less, Greater).
partition([X|Xs], Y, Less, [X|Greater]) :-
    X >= Y,
    partition(Xs, Y, Less, Greater).

% 2.3
% eval_bool(Exp, Env, Res)
% Res is the result of evaluating the boolean expression Exp in the environment Env
eval_bool(true, _, true).
eval_bool(false, _, false).
eval_bool(X > Y, Env, true) :-
    eval_num(X, Env, Xn),
    eval_num(Y, Env, Yn),
    Xn > Yn.
eval_bool(X > Y, Env, false) :-
    eval_num(X, Env, Xn),
    eval_num(Y, Env, Yn),
    Xn =< Yn.

% eval_num(Exp, Env, V)
% V is the result of evaluating the numeric expression Exp in the environment Env
eval_num(id(I), [I = V|_], V).
eval_num(id(I), [J = _|Es], V) :- dif(I, J), eval_num(id(I), Es, V).
eval_num(num(N), _, N).
eval_num(X + Y, Env, V) :-
    eval_num(X, Env, Xn),
    eval_num(Y, Env, Yn),
    V is Xn + Yn.
eval_num(X - Y, Env, V) :-
    eval_num(X, Env, Xn),
    eval_num(Y, Env, Yn),
    V is Xn - Yn.
eval_num(X * Y, Env, V) :-
    eval_num(X, Env, Xn),
    eval_num(Y, Env, Yn),
    V is Xn * Yn.

% contains_id(I, Env)
% Does the environment I contain a value for the identifier I?
contains_id(X, [X = _|_]).
contains_id(X, [_|Xs]) :- contains_id(X, Xs).

% replace_val(I, V, Env, NewEnv)
% if the environment Env contains a value for the identifier I then NewEnv is
% the same environment but with the value for I replaced with V
replace_val(I, V, [I = _|Env], [I = V|Env]).
replace_val(I, V, [E|Env], [E|NewEnv]) :- replace_val(I, V, Env, NewEnv).

% execute(Prog, Env, NewEnv)
% NewEnv is the result of executing the program Prog in the environment Env
execute(skip, Env, Env).
execute(set(id(I), E), Env, NewEnv) :-
    contains_id(I, Env),
    replace_val(I, V, Env, NewEnv),
    eval_num(E, Env, V).
execute(set(id(I), E), Env, [I = V | Env]) :-
    \+contains_id(I, Env),
    eval_num(E, Env, V).
execute(if(B, C, _), Env, NewEnv) :-
    eval_bool(B, Env, true),
    execute(C, Env, NewEnv).
execute(if(B, _, C), Env, NewEnv) :-
    eval_bool(B, Env, false),
    execute(C, Env, NewEnv).
execute(while(B, C), Env, NewEnv) :-
    eval_bool(B, Env, true),
    execute(C, Env, Env2),
    execute(while(B, C), Env2, NewEnv).
execute(while(B, _), Env, Env) :-
    eval_bool(B, Env, false).
execute(seq(C1, C2), Env, NewEnv) :-
    execute(C1, Env, Env2),
    execute(C2, Env2, NewEnv).

% 2.4
% member_of_either(X, Xs, Ys)
% X is a member of either Xs or Ys
member_of_either(X, Xs, _) :- member(X, Xs).
member_of_either(X, _, Xs) :- member(X, Xs).

% union(Xs, Ys, XYs)
% XYs is the union of the sets Xs and Ys
% (the union of two sets is a set of all elements that are in either set)
union(Xs, Ys, XYs) :-
    setof(X, member_of_either(X, Xs, Ys), XYs).

% intersection(Xs, Ys, XYs)
% XYs is the intersection of the sets Xs and Ys
% (the intersection of two sets is the set of all elements that are in both sets)
intersection(Xs, Ys, XYs) :-
    setof(X, (member(X, Xs), member(X, Ys)), XYs).

% subset(Xs, Ys)
% Xs is a subset of Ys
subset([], []).
subset([X|Xs], [X|Ys]) :- subset(Xs, Ys).
subset(Xs, [_|Ys]) :- subset(Xs, Ys).

% powerset(Xs, Power)
% Power is the powerset of the set Xs
% Assuming that Xs is a set (ordered list without duplicates)
powerset(Xs, Power) :- setof(S, subset(S, Xs), Power).

