:- use_module(library(clpfd)).

% container(B,M,D)
% Container B takes M people and D time to unload
container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

% on(B1, B2)
% Container B1 is on top of B2
on(a,d).
on(b,c).
on(c,d).

% tasks(Tasks, Starts, Workers, Cost)
% Tasks is a list of unloading tasks with the start times Starts requiring
% Workers number of workers and costing Cost in total
tasks(Tasks, Starts, End, Workers, Cost) :-
    findall(task(_, D, _, M, B), container(B, M, D), Tasks),
    maplist(task_start, Tasks, Starts),
    Starts ins 0..100,
    Workers in 0..100,
    indomain(Workers),
    Cost #= Workers * End,
    forall(on(B1, B2), constrain_edge_times(Tasks, B1, B2)),
    cumulative(Tasks, [limit(Workers)]),
    foldl(max_end, Tasks, 0, End).

% constrain_edge_times(Tasks, B1, B2)
% Because B1 is on top of B2 the unloading of B1 has to finish before B2 can be unloaded.
constrain_edge_times(Tasks, B1, B2) :-
    member(task(_, _, EndB1, _, B1), Tasks),
    member(task(StartB2, _, _, _, B2), Tasks),
    EndB1 #=< StartB2.

% task_start(T, S)
% S is the start time of the task T
task_start(task(Start, _, _, _, _), Start).

% task_end(T, E)
% E is the end time of the task T
task_end(task(_, _, End, _, _), End).

% max_end(T, E0, E)
% E is the maximum of E0 and the end time of the task T
max_end(T, E0, E) :-
    task_end(T, End),
    E #= max(E0, End).

% minimum_schedule(Tasks, Starts, Workers, Cost)
% Tasks is a list of unloading tasks with the start times Starts requiring
% Workers number of workers and costing Cost in total. Cost is as low as possible
minimum_schedule(Tasks, Starts, End, Workers, Cost) :-
    tasks(Tasks, Starts, End, Workers, Cost),
    once((labeling([min(Cost)], [Cost]), label(Starts))).
