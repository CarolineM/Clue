%Game database
%-------------------------------
%possible suspects
:- dynamic suspect/1.

suspect(mustard).
suspect(scarlet).
suspect(plum).
suspect(green).
suspect(white).
suspect(peacock).

suspects(X) :- findall(X0, suspect(X0), X).

%possible weapons
:- dynamic weapon/1.

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

weapons(X) :- findall(X0, weapon(X0), X).

%possible rooms
:- dynamic room/1.

room(kitchen).
room(dining).
room(lounge).
room(hall).
room(study).
room(library).
room(billiard).
room(conservatory).

rooms(X) :- findall(X0, room(X0), X).

%players are numbers from 0 - 5. 
%Assumes we are player zero and then counts up going clockwise.
:- dynamic player/1.

player(0).

%all the players in a sorted list
players(Y) :- findall(X0, player(X0), X),
			  sort(X, Y).

%represents the cards a player can have
%cards are not listed in the possible rooms/weapons/suspects
:- dynamic card/1.

cards(X) :- findall(X0, card(X0), X).

%relationship of players to cards
:- dynamic has_card/2.

%relationship of players to question parts
:- dynamic asked_question/2.

%returns all the items that a specific player asked about
questions_about(P, X) :- findall(X0, asked_question(P, X0), X).

%move not possible answers here so they are not lost
:- dynamic not_possible/1.
%lists impossible solutions
impossible(X) :- findall(X0, not_possible(X0), X).

%resets the database
clear_all :- abolish(has_card/2),
			 abolish(card/1),
			 abolish(player/1),
			 abolish(room/1),
			 abolish(suspect/1),
			 abolish(player/1),
			 abolish(asked_question/2),
			 abolish(not_possible/1),
			 [clue]. % need to reload file.

%Database modifiers			 
%-----------------------------------

%sets the not_possible atom
set_notpossible(X) :- assert(not_possible(X)).

%expects the number of players, including us. 
%Adds them to the database. 
%Cannot be less than two players.
gen_players(X) :- Y is X - 1, !,
				  Y < 6, 
				  Y > 0, !,
				  assert(player(Y)),
				  gen_players(Y).

%remove items from the database
remove_room(X) :- retract(room(X)),
				  set_notpossible(X).
remove_suspect(X) :- retract(suspect(X)),
					 set_notpossible(X).
remove_weapon(X) :- retract(weapon(X)),
					set_notpossible(X).

%creates card, associates with player and removes card from possible
setcard(P, X) :- room(X), !,
				 player(P), !,
				 remove_room(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))).
setcard(P, X) :- weapon(X), !,
				 player(P), !,
				 remove_weapon(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))).
setcard(P, X) :- suspect(X), !,
				 player(P), !,
				 remove_suspect(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))).

%takes one part of the three part question and assocaites 
%it with the player who asked it. If X is not part of a 
%possible solution, it is ignored.
set_question(P, X) :- room(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).
set_question(P, X) :- weapon(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).
set_question(P, X) :- suspect(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).

%print functions
%------------------------------------
%prints the entire database
printdatabase :- printpossible,
				 write('Player info:'), nl,
				 write('***************'), nl,
				 printplayerinfo.


%prints a list
printlist([]) :- nl.
printlist([H | T]) :-
	write(H), nl,
	printlist(T).

%prints the possible options for an accusation
printpossible :-
	write('Possible killers:'), nl,
	write('******************'), nl,
	suspects(S),
	printlist(S),
	write('Possible weapons:'), nl,
	write('******************'), nl,
	weapons(W),
	printlist(W),
	write('Possible rooms:'), nl,
	write('******************'), nl,
	rooms(R),
	printlist(R).

%prints all the info known about a player
printplayerinfo :- players(X),
				   printinfoforplayers(X).

% helper to print cards for every player
printinfoforplayers([H | T]) :- printinfo(H),
								printinfoforplayers(T).

%prints the cards for a player
printinfo(P) :- findall(X0, has_card(P, X0), X),
				 write('Player '), write(P), nl,
				 write('******************'), nl,
				 write(' has:'), nl,
				 printlist(X),
				 write('And probably does not have:'), nl,
				 common_questions(P, Y),
				 printlist(Y),
				 write('And asked about:'), nl,
				 questions_about(P, Q),
				 printlist(Q).


%helpers
%-----------------------------------------------------------

%filters items in a list based on if they are possible solutions
filter_relevant([], []) :- !.
filter_relevant([H | T], [H | Result]) :- room(H), !,
								          filter_relevant(T, Result).
filter_relevant([H | T], [H | Result]) :- weapon(H), !,
								          filter_relevant(T, Result).
filter_relevant([H | T], [H | Result]) :- suspect(H), !,
								          filter_relevant(T, Result).
filter_relevant([_ | T], Result) :- filter_relevant(T, Result).

%filters rooms out of a list
filter_room([], []) :- !.
filter_room([H | T], Result) :- room(H), !,
								filter_room(T, Result).
filter_room([H | T], [H | Result]) :- filter_room(T, Result).

%returns a list of things that a player asked about more than once 
%that are possible solutions. ignores rooms because asking about a 
%room depends on board position
common_questions(P, Result) :- questions_about(P, X),
							   filter_relevant(X, Y),
							   filter_room(Y, Z),
                               list_dups(Z, Result).

%lists duplicate items in a list
%there will be an entry in the list for every item after it appears once
list_dups([], []) :- !.
list_dups([H | T], [H | Result]) :- member(H, T), !,
									list_dups(T, Result).
list_dups([_ | T], Result) :- list_dups(T, Result).

%returns a list of tuples with a possible [answer, score]
high_prob_ans(Result) :- players(Y), !,
						 all_common_q(Y, R),
						 filter_relevant(R, R1),
						 build_prob_tuples(R1, R1, R2),
						 sort(R2, Result).

%returns a list of the questions aked by all players. Ignores rooms.
all_common_q([], []) :- !.
all_common_q([H | T], Result) :- questions_about(H, X), !,
								 filter_room(X, R),
								 all_common_q(T, R2),
								 append(R, R2, Result).

%builds a list of tuples of the form [answer, prob].
build_prob_tuples([], _, []) :- !.
build_prob_tuples([H | T], L, [[H, Count] | Result]) :- occurrences(L, H, Count), !,
														build_prob_tuples(T, L, Result).
										 

% counts the number of occurences of a value in a list
occurrences(List, Value, Count) :- occurrences(List, Value, 0, Count).
occurrences([], _, Count, Count).
occurrences([X | Xs], Value, Acc, Count) :- X == Value, !,
										    NAcc is Acc + 1, !,
										    occurrences(Xs, Value, NAcc, Count).
occurrences([_ | Xs], Value, Acc, Count) :- occurrences(Xs, Value, Acc, Count).


