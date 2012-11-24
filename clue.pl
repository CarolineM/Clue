%Game database
%-------------------------------
%possible suspects
:- dynamic suspect/1.

suspects(X) :- findall(X0, suspect(X0), X).

%possible weapons
:- dynamic weapon/1.

weapons(X) :- findall(X0, weapon(X0), X).

%possible rooms
:- dynamic room/1.

rooms(X) :- findall(X0, room(X0), X).

%players are numbers from 0 - 5. 
%Assumes we are player zero and then counts up going clockwise.
:- dynamic player/1.
% player(0). unless this is neccessary, because the 'cards no one has' method compares cordinallity of players to the cordinallity of cards that players dont have. having a extra player 0 screws with the comparison.

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

%player has one of 3 cards, based on them showing 
%something in response to question
:- dynamic has_one_of/2.

player_has_one_of(P, X) :- findall(X0, has_one_of(P, X0), X).


% 'does not have' predicate based on who doesn't show
:- dynamic does_not_have/2.

player_does_not_have(P, Y) :- findall(X0, does_not_have(P, X0), X),
							  sort(X, Y).


% case where does_not_have is true for every player
no_one_has(X) :- suspect(X),
                 aggregate_all(count, player(_), Cp),
                 aggregate_all(count,does_not_have(_, X), C), C =:=Cp.
cards_no_one_has(X) :- findall(X0, no_one_has(X0), X).

%TODO 
%checkwin function


%game playing predicates
%---------------------------------------------------------

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
			 [clue]. % need to reload file.

%builds the database and starts the game sequence
startgame :- assert(suspect(mustard)),
			 assert(suspect(scarlet)),
			 assert(suspect(plum)),
			 assert(suspect(green)),
			 assert(suspect(white)),
			 assert(suspect(peacock)),
			 assert(weapon(rope)),
			 assert(weapon(pipe)),
			 assert(weapon(knife)),
			 assert(weapon(wrench)),
			 assert(weapon(candlestick)),
			 assert(weapon(pistol)),
			 assert(room(kitchen)),
			 assert(room(dining)),
			 assert(room(lounge)),
			 assert(room(hall)),
			 assert(room(study)),
			 assert(room(library)),
			 assert(room(billiard)),
			 assert(room(conservatory)),
			 opening_sequence, !,
			 ask_cards, 
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
			 get_cards(X).
ask_cards :- write('please enter a number'), nl,
			 ask_cards.

get_cards(0) :- write('your cards are entered in the database'), nl.
get_cards(N) :- nl, write('enter a card:'), nl,
			    read(Card),
			    possible(Card),
			    setcard(0, Card),
			    Nn is N - 1,
			    get_cards(Nn).
get_cards(N) :- write('that was not a possible card.'), nl,
				get_cards(N).

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
turn_loop(X, L) :- write('it is player '), write(X), write('s turn'), nl,
				   write('enter each part of the question that was asked if no question asked, type \'skip\':'), nl,
				   write('to see what is in the database, type \'print\''), nl,
                   read(Q0),
				   read_data(X,L, Q0),
				   checkall_has_card, !, %checks if we can say a player has a card TODO could do more with probablity
				   Nx is X + 1, !,
				   Mx is mod(Nx, L), !,
				   turn_loop(Mx, L).

turn_loop(_, _) :- write('there was a problem with your input.'), nl, %TODO it is kind of annoying to have to reenter everything
                   write('do you want to exit? y/n'), nl,
                   read(A),
                   A == 'y', !,
				   endgame.

turn_loop(X, L) :- turn_loop(X, L).


read_data(_, _, 'skip').
read_data(_, _, 'print') :- printdatabase.
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
								   write('which card did you see?'), nl,
								   read(Card),
                				   set_q_all_rel(Card, S), !.
turn_logic(X, S, Q0, Q1, Q2, L) :- players_do_not_have_card(X, S, Q0, Q1, Q2, L), !,
                				   set_q_all_rel(X, Q0, Q1, Q2, S), !.


% add players who do not have these cards asked by S and shown by E
players_do_not_have_card(S, E, C1,C2,C3,L):-
			player(E), !,
            Sx is S + 1,
            K is mod(Sx, L),
            K \= E , !,
            assert(does_not_have(Sx, C1)),
            assert(does_not_have(Sx, C2)),
            assert(does_not_have(Sx, C3)),
            players_do_not_have_card(Sx, E, C1,C2,C3,L), !.

players_do_not_have_card(S, E, _,_,_,L):-
			player(E), !,
            Sx is S + 1,
            K is mod(Sx, L),
            K =:= E, !.

players_do_not_have_card(S, 'none', C1,C2,C3,_):-
			players(X),
			delete(X, S, Y),
            cycle_players(Y, C1, C2, C3).

cycle_players([], _, _, _) :- !.
cycle_players([H | T], Q0, Q1, Q2) :- assert(does_not_have(H, Q0)),
            					 	  assert(does_not_have(H, Q1)),
            					 	  assert(does_not_have(H, Q2)),
            					 	  cycle_players(T, Q0, Q1, Q2).

%Database modifiers
%-----------------------------------

%possible answers
possible(X) :- room(X), !.
possible(X) :- weapon(X), !.
possible(X) :- suspect(X), !.

%all
all(X) :- possible(X), !.
all(X) :- card(X), !.

%expects the number of players, including us. 
%Adds them to the database. 
gen_players(0) :- !.
gen_players(X) :- Y is X - 1,
				  Y < 6, !,
				  assert(player(Y)),
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
cycleassert([H | T], C) :- assert(does_not_have(H, C)), !,
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
check_all_hasoneof([X | _], C, P) :- player_does_not_have(P, Nh),
									 subtract(X, Nh, Y),
									 subtract(Y, C, [H | T]),
					 				 length([H | T], Len),
					 				 write([H|T]), nl,
					 				 Len =:= 1, !,
					 				 write('I have deduced that player '), write(P), write(' has this card: '), write(H), nl,
					 				 write('It is being added to the database.'), nl,
					 				 setcard(P, H).
check_all_hasoneof([_ | Xs], C, P) :- check_all_hasoneof(Xs, C, P).


%print functions
%------------------------------------
%prints the entire database
printdatabase :- printpossible,
				% printprobable,
				 write('Player info:'), nl,
				 write('***************'), nl,
				 printplayerinfo.

%prints a list
printlist([]) :- !.
printlist([H | T]) :-
	write(H), write(', '),
	printlist(T).



printprobable :- 
	write('Probability scores:'), nl,
	high_prob_ans(Result), %TODO this is wrong because it is not filtering known cards
	printlist(Result), !.


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
				 write('And probably does not have--| '),
				 common_questions(P, Y),
				 printlist(Y), nl,
				 write('And asked about:--------------| '),
				 questions_about(P, Q),
				 printlist(Q), nl, !.


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

%returns a sorted list of tuples with a possible [answer, score]
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
										 

%counts the number of occurences of a value in a list
occurrences(List, Value, Count) :- occurrences(List, Value, 0, Count).
occurrences([], _, Count, Count).
occurrences([X | Xs], Value, Acc, Count) :- X == Value, !,
										    NAcc is Acc + 1, !,
										    occurrences(Xs, Value, NAcc, Count).
occurrences([_ | Xs], Value, Acc, Count) :- occurrences(Xs, Value, Acc, Count).




