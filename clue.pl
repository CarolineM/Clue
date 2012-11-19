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

players(X) :- findall(X0, player(X0), X).

%-----------------------------------

%prints a list
printlist([]) :- nl.
printlist([H | T]) :-
	write(H), nl,
	printlist(T).

%prints the entre database 
printpossible :-
	write('POSSIBLE KILLERS:'), nl,
	write('******************'), nl,
	suspects(S),
	printlist(S),
	write('POSSIBLE WEAPONS:'), nl,
	write('******************'), nl,
	weapons(W),
	printlist(W),
	write('POSSIBLE ROOMS:'), nl,
	write('******************'), nl,
	rooms(R),
	printlist(R).

%remove items from the database
remove_room(X) :- retract(room(X)).
remove_suspect(X) :- retract(suspect(X)).
remove_weapon(X) :- retract(weapon(X)).

%expects the number of players, including us. 
%Adds them to the database. 
%Cannot be less than two players.
gen_players(X) :- Y is X - 1, !,
				  Y < 6, 
				  Y > 0, !,
				  assert(player(Y)),
				  gen_players(Y).   