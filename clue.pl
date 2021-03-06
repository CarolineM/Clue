%Game database
%-------------------------------

%possible suspects
:- dynamic suspect/1.

add_suspect(S) :- suspect(S).
add_suspect(S) :- assert(suspect(S)).

suspects(X) :- findall(X0, suspect(X0), X).

%possible weapons
:- dynamic weapon/1.

add_weapon(W) :- weapon(W).
add_weapon(W) :- assert(weapon(W)).

weapons(X) :- findall(X0, weapon(X0), X).

%possible rooms
:- dynamic room/1.

add_room(R) :- room(R), !.
add_room(R) :- assert(room(R)).

rooms(X) :- findall(X0, room(X0), X).

%players are numbers from 0 - 5. 
%Assumes we are player zero and then counts up going clockwise.
:- dynamic player/1.

add_player(P) :- player(P), !.
add_player(P) :- assert(player(P)).

%all the players in a sorted list
players(Y) :- findall(X0, player(X0), X),
			  sort(X, Y).

%represents the cards a player can have
%cards are not listed in the possible rooms/weapons/suspects
:- dynamic card/1.

cards(X) :- findall(X0, card(X0), X).

%relationship of players to cards
:- dynamic has_card/2.

player_has_card(P, X) :- findall(X0, has_card(P, card(X0)), X).

%relationship of players to question parts
:- dynamic asked_question/2.

%returns all the items that a specific player asked about
questions_about(P, X) :- findall(X0, asked_question(P, X0), X).

%player has one of 3 cards, based on them showing 
%something in response to question
:- dynamic has_one_of/2.

add_has_one_of(P, C) :- has_one_of(P, C), !.
add_has_one_of(P, C) :- assert(has_one_of(P, C)), !.

player_has_one_of(P, X) :- findall(X0, has_one_of(P, X0), X).


% 'does not have' predicate based on who doesn't show
:- dynamic does_not_have/2.

add_does_not_have(P, C) :- player(P), all(C), does_not_have(P, C), !.
add_does_not_have(P, C) :- player(P), all(C), !, assert(does_not_have(P, C)), !.

player_does_not_have(P, Y) :- findall(X0, does_not_have(P, X0), X),
							  sort(X, Y).

% case where does_not_have is true for every player
check_no_one_has(X) :- possible(X), !,
                	   aggregate_all(count, player(_), Cp),
                 	   aggregate_all(count, does_not_have(_, X), C), C =:= Cp,
                 	   add_no_one_has(X).

check_all_noonehas([]) :- !.
check_all_noonehas([H | T]) :- check_no_one_has(H), !,
							   check_all_noonehas(T).
check_all_noonehas([H | T]) :- not(check_no_one_has(H)), !,
							   check_all_noonehas(T).

add_no_one_has(X) :- add_final_answer(X), !.
add_no_one_has(X) :- add_final_answer(X),
					 write('one part of the final answer is: '), write(X), nl, !.


%when we know for sure a card is part of the answer
:- dynamic final_room/1.
:- dynamic final_weapon/1.
:- dynamic final_suspect/1.

add_final_answer(X) :- final_room(X), !.
add_final_answer(X) :- room(X), !, assert(final_room(X)), !.
add_final_answer(X) :- final_weapon(X), !.
add_final_answer(X) :- weapon(X), !, assert(final_weapon(X)), !.
add_final_answer(X) :- final_suspect(X), !.
add_final_answer(X) :- suspect(X), !, assert(final_suspect(X)), !.

final_answers(X) :- findall(R0, final_room(R0), R), 
					findall(S0, final_suspect(S0), S),
					findall(W0, final_weapon(W0), W),
					append(R, S, X0),
					append(X0, W, X).


%the number of cards a player has
:- dynamic num_cards/2.

add_num_cards(P, _) :- num_cards(P, _), 
					   write('no new information was added because the number is already set'), nl, !.
add_num_cards(P, N) :- assert(num_cards(P, N)), !,
					   write('player '), write(P), write(' has '), write(N), write(' has been recorded.'), nl, !.

player_num_cards(P, X) :- findall(X0, num_cards(P, X0), X).


%game playing predicates
%---------------------------------------------------------
checkwin :- final_answers(X),
			length(X, L),
			L =:= 3,
			final_room(R),
			final_suspect(S),
			final_weapon(W),
			write('*************************************************'), nl,
			write(S), write(' did it, in the '), write(R), write(' with the '), write(W), write('!'), nl,
			write('*************************************************'), nl.

checkwin :- !.	

%resets the database
endgame  :-  abolish(has_card/2),
			 abolish(card/1),
			 abolish(player/1),
			 abolish(room/1),
			 abolish(suspect/1),
			 abolish(weapon/1),
			 abolish(player/1),
			 abolish(asked_question/2),
			 abolish(does_not_have/2),
             abolish(has_one_of/2),
             abolish(final_room/1),
             abolish(final_weapon/1),
             abolish(final_suspect/1),
             abolish(num_cards/2),
			 [clue]. % need to reload file.

%builds the database and starts the game sequence
startgame :- add_suspect(mustard),
			 add_suspect(scarlet),
			 add_suspect(plum),
			 add_suspect(green),
			 add_suspect(white),
			 add_suspect(peacock),
			 add_weapon(rope),
			 add_weapon(pipe),
			 add_weapon(knife),
			 add_weapon(wrench),
			 add_weapon(candlestick),
			 add_weapon(pistol),
			 add_room(kitchen),
			 add_room(dining),
			 add_room(lounge),
			 add_room(hall),
			 add_room(study),
			 add_room(library),
			 add_room(billiard),
			 add_room(ballroom),
			 add_room(conservatory),
			 opening_sequence, !,
			 ask_cards, !,
			 start_turn_rotation, !.

%loops on fail
startgame :- endgame,
			 startgame.

%sets up the number of players
opening_sequence :- write('how many players are going to play?'), nl,
			 		read(X),
			 		number(X), 
			 		X < 7, X > 2, 
			 		gen_players(X), !, 
			 		Y is X - 1, 
			 		write('there are players 0 - '), write(Y), write(' in the database'), nl,
			 		write('you are player 0'), nl,
			 		write('the other players are numbered clockwise from your current position'), nl.

%fail case
opening_sequence :- write('please enter an number between 3 and 6'), nl,
					opening_sequence.


%gets player cards
ask_cards :- write('how many cards do you have?'), nl,
			 read(X),
			 number(X), !,
			 write('here is a list of possible cards:'), nl,
			 printpossible,
			 get_cards(X),
			 setall_doesnothave(0).
ask_cards :- write('please enter a number'), nl,
			 ask_cards.

get_cards(0) :- write('your cards are entered in the database'), nl.
get_cards(N) :- nl, write('enter a card:'), nl,
			    read(Card),
			    possible(Card),
			    setcard(0, Card), !,
			    Nn is N - 1,
			    get_cards(Nn).
get_cards(N) :- write('that was not a possible card.'), nl,
				get_cards(N).

%sets all the remaining cards to does_not_have for player
setall_doesnothave(P) :- all_possible(X),
					     setall_doesnothave(X, P).

setall_doesnothave([], _) :- !.
setall_doesnothave([H | T], P) :- add_does_not_have(P, H),
							      setall_doesnothave(T, P). 

%checks if max cards is reached for all players
check_maxcards :- players(P),
				  check_maxcards(P).
check_maxcards([]) :- !.
check_maxcards([H | T]) :- reached_max(H),
						   check_maxcards(T).

reached_max(P) :- num_cards(P, X),
				  player_has_card(P, Y),
				  length(Y, X),
				  setall_doesnothave(P), !.
reached_max(_) :- !.


%gets the starting player and starts the game loop
start_turn_rotation :- write('who has the first turn?'), nl,
					   write('turns must move clockwise, in order'), nl,
					   read(Start),
					   player(Start), !,
					   players(X),
					   length(X, N),
					   turn_loop(Start, N).
start_turn_rotation :- write('that is not a valid player. here is a list of players:'), nl,
					   players(X),
					   write(X), nl,
					   start_turn_rotation.


%runs the rest of the game adding values to the database every turn
turn_loop(X, L) :- final_answers(A),
				   printfinal(A),	
				   write('it is player '), write(X), write('s turn'), nl,
				   write('******************************************************'), nl,
				   write('enter each part of the question that was asked if no question asked, type \'skip\':'), nl,
				   write('to see what is in the database, type \'print\''), nl,
				   write('to tell me how many cards a player has, type \'addcards\''), nl,
				   write('if you see someone\'s card, you can add it to the database by typing \'sawcard\''), nl,
				   write('type \'quit\' to end the game.'), nl,
				   write('******************************************************'), nl,
                   read(Q0), !,
				   read_data(X, L, Q0), !,
				   check_maxcards, !,
				   checkall_has_card, 
				   all_possible(C),
				   check_all_noonehas(C),
				   checkoneleft, 
				   prompt_room,
				   prompt_suspect,
				   prompt_weapon,
				   check_all_onehas(C),
				   checkwin, !,
				   Nx is X + 1, !,
				   Mx is mod(Nx, L), !,
				   turn_loop(Mx, L), !.

turn_loop(_, _) :- write('there was a problem with your input.'), nl, fail.


turn_loop(X, L) :- turn_loop(X, L).


read_data(_, _, 'skip').
read_data(X, L, 'print') :- printdatabase, turn_loop(X, L).
read_data(X, L, 'addcards') :- addcards, turn_loop(X, L).
read_data(X, L, 'sawcard') :- sawcard, turn_loop(X, L).
read_data(_, _, 'quit') :- write('ending game...'), nl, endgame, fail.
read_data(X, L, Q0) :- 
                all(Q0),
                read(Q1),
                all(Q1),
                read(Q2),
                all(Q2),
                write('which player showed a card? type \'none\' if no one shows'), nl,
                read(S),
                turn_logic(X, S, Q0, Q1, Q2, L).

%different logic for player zero turn
turn_logic(0, S, Q0, Q1, Q2, L) :- players_do_not_have_card(0, S, Q0, Q1, Q2, L), !,
								   write('which card did you see? type \'none\' if no one shows'), nl, 
								   read(Card),
                				   set_q_all_rel(Card, S), !.
turn_logic(X, S, Q0, Q1, Q2, L) :- players_do_not_have_card(X, S, Q0, Q1, Q2, L), !,
                				   set_q_all_rel(X, Q0, Q1, Q2, S), !.


% add players who do not have these cards asked by S and shown by E
players_do_not_have_card(S, E, C1, C2, C3, L):-
			player(E),
            Sx is S + 1,
            K is mod(Sx, L),
            K \= E , !,
            add_does_not_have(K, C1),
            add_does_not_have(K, C2),
            add_does_not_have(K, C3),
            players_do_not_have_card(K, E, C1, C2, C3, L).

players_do_not_have_card(S, E, _,_,_,L):-
			player(E),
            Sx is S + 1,
            K is mod(Sx, L),
            K =:= E, !.

players_do_not_have_card(S, 'none', C1,C2,C3,_):-
			players(X),
			delete(X, S, Y),
            cycle_players(Y, C1, C2, C3).

cycle_players([], _, _, _) :- !.
cycle_players([H | T], Q0, Q1, Q2) :- add_does_not_have(H, Q0),
            					 	  add_does_not_have(H, Q1),
            					 	  add_does_not_have(H, Q2),
            					 	  cycle_players(T, Q0, Q1, Q2).

%Database modifiers
%-----------------------------------

%add number of cards for a player 
addcards :- write('which player?'), nl,
			read(P),
			player(P),
			write('how many cards do they have?'), nl,
			read(Nc),
			integer(Nc),
			add_num_cards(P, Nc), !.
addcards :- write('operation failed. please try again.'), nl, !.

%add card for a player 
sawcard :- write('which player?'), nl,
			read(P),
			player(P),
			write('which card?'), nl,
			read(Nc),
			possible(Nc),
			setcard(P, Nc).
sawcard :- write('operation failed. please try again.'), nl, !.


%possible answers
possible(X) :- room(X).
possible(X) :- weapon(X).
possible(X) :- suspect(X).

all_possible(X) :- findall(X0, possible(X0), X).

%all
all(X) :- possible(X).
all(X) :- card(X).

%expects the number of players, including us. 
%Adds them to the database. 
gen_players(0) :- !.
gen_players(X) :- Y is X - 1,
				  Y < 6, !,
				  add_player(Y),
				  gen_players(Y).

%remove items from the database
remove_room(X) :- retract(room(X)).
remove_suspect(X) :- retract(suspect(X)).
remove_weapon(X) :- retract(weapon(X)).

%creates card, associates with player and removes card from possible
setcard(P, X) :- room(X), !,
				 player(P), !,
				 remove_room(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))), !,
				 set_does_not_have_except_p(P, X).
setcard(P, X) :- weapon(X), !,
				 player(P), !,
				 remove_weapon(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))), !,
				 set_does_not_have_except_p(P, X).
setcard(P, X) :- suspect(X), !,
				 player(P), !,
				 remove_suspect(X),
				 assert(card(X)),
				 assert(has_card(P, card(X))), !,
				 set_does_not_have_except_p(P, X).
setcard(_, X) :- card(X), !, write('no card was set because it already exists'), nl.

%sets does not have for all players but P
set_does_not_have_except_p(P, C) :- players(X), delete(X, P, Y), cycleassert(Y, C).

cycleassert([], _) :- !.
cycleassert([H | T], C) :- add_does_not_have(H, C), !,
						   cycleassert(T, C).


%takes one part of the three part question and assocaites 
%it with the player who asked it. 
set_question(P, X) :- room(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).
set_question(P, X) :- weapon(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).
set_question(P, X) :- suspect(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).
set_question(P, X) :- card(X), !,
					  player(P), !,
					  assert(asked_question(P, X)).

%assocaites all the questions with the player who asked and
%sets the card shower to having one of the three questions
set_q_all_rel(Card, S) :- player(S), !,
					      setcard(S, Card).
set_q_all_rel(_, 'none') :- !.
set_q_all_rel(P, Q1, Q2, Q3, S) :- player(P),
								   player(S), !,
								   set_question(P, Q1),
								   set_question(P, Q2),
								   set_question(P, Q3),
								   assert(has_one_of(S, [Q1, Q2, Q3])).
set_q_all_rel(_, _, _, _, 'none') :- !.

checkall_has_card :- players(X),
					 checkall_has_card(X).

checkall_has_card([]) :- !.
checkall_has_card([H | T]) :- check_has_card(H), !,
							  checkall_has_card(T).

%checks if we can deduce that a player has a card and if we can, add it to the database
check_has_card(P) :- player(P),
					 cards(C0), !,
					 player_has_one_of(P, L), !,
					 check_all_hasoneof(L, C0, P).


%helper to recurse through the hasoneof relationships. stops if a matching sequence is found.
check_all_hasoneof([], _, _) :- !.
check_all_hasoneof([X | T], C, P) :- player_has_card(P, Y),
									 subtract(X, Y, Z),
									 length(Z, L),
									 L < 3, !,
									 check_all_hasoneof(T, C, P).
check_all_hasoneof([X | _], C, P) :- player_does_not_have(P, Nh),
									 subtract(X, Nh, Y),
									 subtract(Y, C, [H | T]),
					 				 length([H | T], Len),
					 				 Len =:= 1, !,
					 				 write('I have deduced that player '), write(P), write(' has this card: '), write(H), nl,
					 				 write('It is being added to the database.'), nl,
					 				 setcard(P, H).
check_all_hasoneof([_ | Xs], C, P) :- check_all_hasoneof(Xs, C, P).

checkoneleft :- not(final_weapon(_)),
				weapons(W),
				length(W, L),
				L =:= 1,
				nth0(0, W, I),
				add_final_answer(I),
				write('one part of the final answer is: '), write(I), nl.

checkoneleft :- not(final_room(_)),
				rooms(R),
				length(R, L),
				L =:= 1,
				nth0(0, R, I),
				add_final_answer(I),
				write('one part of the final answer is: '), write(I), nl.

checkoneleft :- not(final_suspect(_)),
				suspects(S),
				length(S, L),
				L =:= 1,
				nth0(0, S, I),
				add_final_answer(I),
				write('one part of the final answer is: '), write(I), nl.

checkoneleft :- !.




%print functions
%------------------------------------
%prints the entire database
printdatabase :- write('Player info:'), nl,
				 write('***************'), nl,
				 printplayerinfo, nl,
				 printpossible,
				 final_answers(X),
				 printfinal(X).	

%prints a list
printlist([]) :- nl, !.
printlist([H | T]) :-
	write(H), write(', '),
	printlist(T).

printfinal([]) :- !.
printfinal(L) :- write('ATTENTION - the following cards are part of the answer:'), nl,
				 printlist(L), !.



%prints the possible options for an accusation
printpossible :-nl,
    write('********************'),nl,
	write('* Possible killers *'), nl,
	write('********************'), nl,
    write('                    '),
	suspects(S),
	printlist(S),
    nl,
    write('********************'),nl,
	write('* Possible weapons *'), nl,
	write('********************'), nl,
    write('                    '),
	weapons(W),
	printlist(W),
    nl,
    write('********************'),nl,
	write('* Possible rooms   *'), nl,
	write('********************'), nl,
    write('                    '),
	rooms(R),
	printlist(R), !.

%prints all the info known about a player
printplayerinfo :- players(X),
				   printinfoforplayers(X), !.

% helper to print cards for every player
printinfoforplayers([]) :- !.
printinfoforplayers([H | T]) :- printinfo(H),
								printinfoforplayers(T).

%prints all of the info we have about a player
printinfo(P) :- findall(X0, has_card(P, X0), X),
				 write('Player '), write(P), nl,
				 write('*******************************'), nl,
				 write('has:--------------------------| '),
				 printlist(X),nl,
				 write('does not have:----------------| '),
				 player_does_not_have(P, Z), 
				 printlist(Z),nl,
				 write('has one of each list:---------| '),
				 player_has_one_of(P, L),
				 printlist(L),nl,
				 write('And asked about:--------------| '),
				 questions_about(P, Q),
				 printlist(Q), nl, 
				 write('has this many cards:----------| '),
				 player_num_cards(P, Nc),
				 write(Nc), nl, !.

%prompts the user to find out if player x has y card 
check_one_has(X) :- possible(X), !,
                	aggregate_all(count, player(_), Cp),
                 	aggregate_all(count, does_not_have(_, X), C), 
                 	NC is C + 1,
                 	NC =:= Cp,
                 	player(P),
                 	not(does_not_have(P, X)),
                 	write('I need to know if player '), write(P), write(' has '), write(X),
                 	write(' can you find out?'), nl.

check_all_onehas([]) :- !.
check_all_onehas([H | T]) :- check_one_has(H), !,
							 check_all_onehas(T).
check_all_onehas([H | T]) :- not(check_one_has(H)), !,
							 check_all_onehas(T).

%promps the user when only two are left in category
prompt_room :- not(final_room(_)),
			   rooms(R),
			   length(R, L),
			   L =:= 2,
			   write('the room is one of '), write(R), write(' try to find out which'), nl, !.
prompt_room :- !.
prompt_weapon :- not(final_weapon(_)),
			   	 weapons(R),
			   	 length(R, L),
			     L =:= 2,
			     write('the weapon is one of '), write(R), write(' try to find out which'), nl, !.
prompt_weapon :- !.
prompt_suspect :- not(final_suspect(_)),
			      suspects(R),
			      length(R, L),
			      L =:= 2,
			      write('the suspect is one of '), write(R), write(' try to find out which'), nl, !.
prompt_suspect :- !.
