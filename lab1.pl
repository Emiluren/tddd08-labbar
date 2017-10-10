% Exercise 1.1
woman(ulrika).
woman(bettan).
man(nisse).
man(peter).
man(bosse).

beautiful(ulrika).
beautiful(nisse).
beautiful(peter).

rich(nisse).
rich(bettan).

strong(bettan).
strong(peter).
strong(bosse).

kind(bosse).

% likes(X, Y)
% Does X like Y?
% All men like beautiful women
likes(X, Y) :- man(X), woman(Y), beautiful(Y).

% Nisse likes all women who like him
likes(nisse, X) :- woman(X), likes(X, nisse).

% Ulrika likes all men who are (1) rich and kind or (2) beautiful and strong
% if they like her.
% If you move the likes requirement before the rich+kind/beautiful+strong requirement the
% program will end up in infinite recursion because Nisse also likes all women who like him
likes(ulrika, X) :- man(X), (rich(X), kind(X); beautiful(X), strong(X)), likes(X, ulrika).

% happy(X)
% Is X happy?
% Rich people are happy
happy(X) :- rich(X).

% Men who are liked by a woman are happy
happy(X) :- man(X), woman(Y), likes(X, Y), likes(Y, X).

% Women who are liked by a man are happy
happy(Y) :- man(X), woman(Y), likes(X, Y), likes(Y, X).

% peopleWhoLike(X, L)
% Find all the people (L) who like X
peopleWhoLike(X, L) :- findall(Y, likes(Y, X), L).

% amountOfPeopleWhoLike(X, N)
% Counts the amount of people (N) who like X
amountOfPeopleWhoLike(X, N) :- peopleWhoLike(X, L), length(L, N).

% Exercise 1.2

% Direct connections
directpath(a, b).
directpath(b, c).
directpath(a, c).
directpath(c, d).
directpath(d, h).
directpath(c, e).
directpath(d, f).
directpath(e, f).
directpath(e, g).
directpath(f, g).

% Nodes are connected if there is a direct connection
% Nodes are also connected if there is a path of connected nodes between them

% path(X, Y)
% Is there a path between X and Y? Either direcly or going through other nodes
path(X, Y) :- directpath(X, Y).
path(X, Y) :- directpath(X, Z), path(Z, Y).

% path(X, Y, L)
% Calculate the path from X to Y
path(X, Y, [X, Y]) :- directpath(X, Y).
path(X, Y, [X | L]) :- directpath(X, Z), path(Z, Y, L).

% npath(X, Y, N)
% The distance between two nodes (X and Y) is the amount of nodes traveled (N)
npath(X, Y, N) :- path(X, Y, L), length(L, N).
