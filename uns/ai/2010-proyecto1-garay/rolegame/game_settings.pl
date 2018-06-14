% Game settings

time_to_think(1).

vision_length(3).

mountain_resting_time(1).

unconscious_time(10).

climbing_time(1).

fight_skill_reward(1).


registration_handler_freq(2).

dice_sides(50).

% Esto implica que si un agente le lleva 50 de fight_skill a otro
% entonces siempre le ganará.

hostel_recovery_rate(20).

%max_stamina(50).

%max_stamina(MS):- n_of_arrows(R), n_of_columns(C),
%                  MS is round(R*C).

:- n_of_arrows(R),
   n_of_columns(C),
   MS is round(R*C/2),
   assert(initial_max_stamina(MS)).

% Deprecated predicate. Only for compatibility reasons (used from JAVA)
max_stamina(MS):- initial_max_stamina(MS).

initial_fight_skill(100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fight_skill_function(+FightsWon, -FightSkill)
%
fight_skill_function(AttacksWon, FightSkill):-
                                initial_fight_skill(InitialFightSkill),
                                FightSkill is floor(InitialFightSkill + 2*sqrt(AttacksWon)).

max_stamina_function(TrainingActions, MaxStamina):-
	                        initial_max_stamina(InitialMaxStamina),
                                MaxStamina is floor(InitialMaxStamina + 2*sqrt(TrainingActions)).

%forbidden_entry_time(FET):- max_stamina(MS),
%                            FET is round(MS/2).

:- initial_max_stamina(MS),
   FET is round(MS/2),
   assert(forbidden_entry_time(FET)).

%wake_up_stamina(20).
%wake_up_stamina(WS):- max_stamina(MS),
%                      WS is round(MS * 0.2).

:- initial_max_stamina(MS),
   WS is round(MS * 0.2),
   assert(wake_up_stamina(WS)).



stamina_cost(attack, 1).
stamina_cost(turn, 1).
stamina_cost(move_fwd_plain, 1).
stamina_cost(move_fwd_mountain, 2).
stamina_cost(pickup, 0).
stamina_cost(drop, 0).

%ags_starting_pos([8,7]).

ags_starting_positions([[[8,7], n], [[8,9], e], [[10,9], s], [[10,7], w], [[11,8], s], [[7,11], n]]).
