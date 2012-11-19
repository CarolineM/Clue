%Game database
%-------------------------------
%suspects
:- dynamic suspect/1.

suspect(mustard).
suspect(scarlet).
suspect(plum).
suspect(green).
suspect(white).
suspect(peacock).

suspects(X) :- findall(X0, suspect(X0), X).

%weapons
:- dynamic weapon/1.

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

weapons(X) :- findall(X0, weapon(X0), X).

%rooms
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

%expects the number of players, including us. 
%Adds them to the database. 
%Cannot be less than two players.
gen_players(X) :- Y is X - 1, !,
				  Y < 6, 
				  Y > 0, !,
				  assert(player(Y)),
				  gen_players(Y).

%represents the cards a player can have
%cards are not listed in the possible rooms/weapons/suspects
:- dynamic card/1.

cards(X) :- findall(X0, card(X0), X).

%relationship of players to cards
:- dynamic has_card/2.

%resets the database
clear_all :- abolish(has_card/2),
			 abolish(card/1),
			 abolish(player/1),
			 abolish(room/1),
			 abolish(suspect/1),
			 abolish(player/1),
			 [clue]. % need to reload file.
%-----------------------------------
%prints the entire database
printdatabase :- printpossible,
				 write('Player info:'), nl,
				 write('***************'), nl,
				 printallcards.


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

%prints all the known cards
printallcards :- players(X),
				 printcardsforplayers(X).

% helper to print cards for every player
printcardsforplayers([H | T]) :- printcards(H),
								 printcardsforplayers(T).

%prints the cards for a player
printcards(P) :- findall(X0, has_card(P, X0), X),
				 write('Player '), write(P), write(' has:'), nl,
				 printlist(X).

%remove items from the database
remove_room(X) :- retract(room(X)).
remove_suspect(X) :- retract(suspect(X)).
remove_weapon(X) :- retract(weapon(X)).

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


