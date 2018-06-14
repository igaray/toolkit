% agenteastar.pl: agent that finds its path using A* search.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A* AGENT

astar(Percept, Action) :-
    choose_action(astar,Percept, Action).

