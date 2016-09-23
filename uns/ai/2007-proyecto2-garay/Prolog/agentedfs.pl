% agentedfs.pl: agent that finds its path using DFS search.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DFS AGENT

dfs(Percept, Action) :-
    choose_action(dfs,Percept, Action).

