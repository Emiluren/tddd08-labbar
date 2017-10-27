
% travellers(C, M)
% C is the amount of cannibals in the boat
% M is the amount of missionaries in the boat
% The boat can take at most 2 people and at least 1
travellers(0, 1).
travellers(1, 0).
travellers(1, 1).
travellers(2, 0).
travellers(0, 2).

% possible_side(C, M)
% 0 =< C =< 3 is the number of cannibals on a side
% 0 =< M =< 3 is the number of missionaries on a side
possible_side(C, M) :- between(0, 3, C), between(0, 3, M).

% safe_side(C, M)
% A side is safe if the cannibals (C) don't outnumber the missionaries (M)
safe_side(side(_, 0)).
safe_side(side(C, M)) :- M > 0, M >= C.

% move(Side, Side', Boat)
% Side' is the resulting state when the travellers in Boat are moved from Side
move(side(C, M), side(C_, M_), boat(Dc, Dm)) :-
    possible_side(C, M),
    possible_side(C_, M_),
    travellers(Dc, Dm),
    C is C_ + Dc,
    M is M_ + Dm.

% possible_state(Side1, Side2) A state has two sides (Side1, Side2) and the
% total number of cannibals/missionaries on both sides is 3
possible_state(side(C1, M1), side(C2, M2)) :-
    possible_side(C1, M1),
    possible_side(C2, M2),
    plus(C1, C2, 3),
    plus(M1, M2, 3).

% transfer(Side1, Side2, Side1_, Side2_) Some characters are moved from side 1
% to side 2. Side1/Side2 is the previous state of side 1/2 and Side1_/Side2_ is
% the new state of side 1/2. The resulting sides have to be safe.
transfer(Side1, Side2, Side1_, Side2_) :-
    %% possible_state(Side1, Side2),
    move(Side1, Side1_, B),
    move(Side2_, Side2, B),
    safe_side(Side1_),
    safe_side(Side2_).

% action(State, State')
% If State is a legal state of the world then State' is a legal next state
% with the boat going from either left to right or right to left
action(state(LSide, RSide, left), state(LSide_, RSide_, right)) :-
    transfer(LSide, RSide, LSide_, RSide_).
action(state(LSide, RSide, right), state(LSide_, RSide_, left)) :-
    transfer(RSide, LSide, RSide_, LSide_).

% not_member(X, Xs)
% X is not an element in Xs
not_member(_, []).
not_member(X, [Y|Xs]) :- dif(X, Y), not_member(X, Xs).

% Initially all three cannibals and missionaries are standing on the left side
initial_state(state(side(3, 3), side(0, 0), left)).

% Exercise 4.2
df_path(X) :- initial_state(S), df_path((S), [S], X).

df_path(state(side(0, 0), side(3, 3), right), Visited, Visited).
df_path(State, Visited, Path) :-
    action(State, State_),
    not_member(State_, Visited),
    df_path(State_, [State_|Visited], Path).
