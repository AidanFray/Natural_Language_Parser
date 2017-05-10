agent:-
	percive(Percept),
	action(Percept),
	agent().

%---------------SYNONYMs---------------------%
synonym(listing, [classifying]).
synonym(prints, [views]).
synonym(moving, [moves,changing, stepping, steps]).
synonym(directory, [folder]).
synonym(types, [cats]).
synonym(higher,[parent]).
%--------------------------------------------%
%
%---------------DICTIONARY-------------------%
det(the).
det(a).
det(that).

adj(current).
adj(more).
adj(fine).
adj(short).
adj(very).
adj(higher).
adj(file).

noun(directory).
noun(detail).
noun(command).
noun(parent).
noun(folder).
noun('08226txt').

verb(viewed).
verb(listing).
verb(moves).
verb(moving).
verb(moves).
verb(classifying).
verb(steps).
verb(stepping).
verb(cats).
verb(prints).
verb(views).
verb(types).
verb(cats).

adverb(none).

prep(up).
prep(in).
prep(that).
prep(to).
prep(out).
%--------------------------------------------%

%====================RULES=============================
rule(r1,
     np2(adj(very),np2(adj(short),np2(noun(command)))),
     listing,
     np2(adj(current),np2(noun(directory))),
     write('ls')).

rule(r2,
     np2(adj(current),np2(noun(directory))),
     viewed,
     np2(adj(more),np2(adj(fine),np2(noun(detail)))),
     write('ls -la')).

rule(r3,
     np2(noun(command)),
     moving,
     np2(adj(higher),np2(noun(directory))),
     write('cd ..')).

rule(r4,
     np2(noun(command)),
     prints,
     np2(adj(current),np2(noun(directory))),
     write('pwd')).

rule(r5, np2(noun(command)),
     types,
     np2(adj(file),np2(noun('08226txt'))),
     write('cat 08226.txt')).

rule(_,_,_,_,
     write('Sorry, I didnt quite get that')).

%--------------------------------------------%
%
percive(Percept) :-
	write('Please enter a sentence: '),nl,nl,

	%Takes the uer input
	read_line_to_codes(user_input, UserInput),

	%Remove fullstop end
	remove_fullstop(UserInput, UserInputNoFull),

	%Converts the input (List of ASCII Codes) to a string
        translate(UserInputNoFull, Out),

	%Checks if the user wants to end
	end(Out),

	%Splits the string using spaces into a list of strings
	split_string(Out, Out1),

	%Translates each item in the list of strings into an atom
	turn_to_atoms(Out1,PerceptBeforeSyn),

	%Synonymn
	synonym_replace(PerceptBeforeSyn, Percept).

action(Percept) :-
	sentence(Percept, Parse),

	%Splits into a NounPhrase and VerbPhrase
	Parse = sentence(NP, VP),

	remove_head(NP, NP1),

	%Takes out the verb from the verbphrase
	VP = vp(verb(Verb), Rest),

	%Removes all det and preps
	remove_head(Rest, NP2),

	%Checks against the rules
	rule(_,NP1, Verb, NP2, Output),

	%Writes out the output
	nl,Output,nl,nl.


%Removes the extra words in scentence
remove_head(Input, Return) :-
	Input = pp(_,Rest),
	remove_head(Rest, Return)
	;
	Input = np(_,Rest),
	remove_head(Rest,Return).

remove_head(Input, Return) :-
	Return = Input.

remove_fullstop([Head|[]], Return) :-
	Head = 46, Return = [] ;
	Return = [Head|[]].

remove_fullstop([Head|Tail], [Head|Return]) :-
	remove_fullstop(Tail, Return).


%-----------------------PERCIVE---------------------------
%So the user can end the loop
end(E) :-
       (member(E, ["end","End","stop","Stop"]), abort) ; true.

%Translate data into a list of strings
translate(H,R) :-
	string_to_list(R,H).

%Converts list of strings to atom
turn_to_atoms([],[]).
turn_to_atoms([H|Tail], [R|Return]) :-
	atom_codes(R, H),
	turn_to_atoms(Tail, Return).

split_string(String, Output) :-
	split_string(String, " ", "", Output).

%Translates words to their base synonyms
synonym_replace([],[]).
synonym_replace([Head|Tail], [NewHead|Return]) :-

	synonym(Root, SL),

	%If it needs replacing
	(member(Head, SL), NewHead = Root, synonym_replace(Tail, Return)) ;

	%If it needs leaving alone
	NewHead = Head, synonym_replace(Tail, Return).


%=======================GRAMMER RULES==============================
%S -> NP VP
sentence(Sentence, sentence(NP,VP)):-
	np(Sentence,NP,Remainder),
	vp(Remainder,VP).

%S -> VP
sentence(Sentence,_) :-
	vp(Sentence,_).

sentence(_,_) :-
	nl,write('Incorrect Phrase!'),
	!,false.

%NP -> Det NP2
np([X|T], np(det(X),NP2), Rem):-
	det(X),
	np2(T,NP2,Rem).

%NP -> NP2
np(Sentence, Parse, Rem):-
	np2(Sentence,Parse,Rem).

%NP -> NP PP
np([X|T], np(NP,PP), Rem):-
	%Makes sure the word is correct
	%If this isn't here the loop continues forever
	(det(X);adj(X);noun(X);prep(X);verb(X)) ->

	%If word in dictionary
	(np(X+T,NP,Rem1),
	 pp(Rem1,PP,Rem))

	;

	%Error output
	!,false.

/*NP2*/
%NP2 -> Noun
np2([H|T], np2(noun(H)),T):-
	noun(H).

%NP2 -> Adj NP2
np2([H|T], np2(adj(H),Rest), Rem):-
	adj(H),
	np2(T,Rest,Rem).

%PP -> Prep NP
pp([H|T], pp(prep(H),Parse), Rem):-
	prep(H),
	np(T,Parse,Rem).


%VP -> Verb
vp([H|[]], (verb(H))) :-
	verb(H).

%VP -> Verb Adverb NP
vp([H1,H2|Rest], (verb(H1), adverb(H2), RestParsed)) :-
        verb(H1),
        adverb(H2),
        np(Rest,RestParsed,_).

%VP -> Verb Adverb
vp([H1,H2|[]], vp(verb(H1), adverb(H2))) :-
	verb(H1),
	adverb(H2).

%VP -> VP PP
vp([H|Rest], vp(verb(H), RestParsed)) :-
	verb(H),
	pp(Rest,RestParsed,_).

%VP -> Verb NP
vp([H|Rest], vp(verb(H), RestParsed)) :-
	verb(H),
	np(Rest,RestParsed,_).
%===========================================================









