% -----------------------------------------------------------------
% Natural Language Processing
% Written by Dat Huynh
% AI Prolog program that process simple English sentences
% Using Backtracking Algorithm to find preference
% of a personal pronoun or a possessive pronoun
% -----------------------------------------------------------------

:- dynamic(history/1).

sentence(VP) --> noun_phrase(Number, Actor), verb_phrase(Actor, Number, VP).

noun_phrase(plural, set(NP1, NP2)) --> np1(_, NP1), [and], noun_phrase(_, NP2).
noun_phrase(Number, NP1) --> np1(Number, NP1).

np1(Number, thing(Noun, Properties)) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun).
np1(Number, thing(Noun, [PP | Properties])) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun),
	pp(Number, PP).
np1(Number, thing(Name, [])) -->
	proper_noun(Number, _, Name).
np1(Number, personal(Pro)) -->
	pronoun(Number, _, Pro).
np1(Number1, possessive(Pos, NP)) -->
	possessive_pronoun(Number1, _, Pos), noun_phrase(_, NP).
np1(Number, object(Noun)) -->
	num(Number), noun(Number, Noun).

adjp([Adj]) --> adjective(Adj).
adjp([]) --> [].

verb_phrase(Actor, Number, event(V, [actor(Actor) | Adv])) -->
	verb(Number, V),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP) | Adv])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP), PP])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	pp(Number, PP).
verb_phrase(Actor, Number, event(V, [actor(Actor), PP])) -->
	verb(Number, V),
	pp(_, PP).

pp(_, PP) --> prep(NP, PP), noun_phrase(_, NP).

% The next set of rules represent the lexicon

prep(NP, object(NP)) --> [of].
prep(NP, object(NP)) --> [to].
prep(NP, instrument(NP)) --> [with].
prep(NP, object(NP)) --> [in].
prep(NP, object(NP)) --> [for].

determiner(singular, det(a)) --> [a].
determiner(_, det(the)) --> [the].
determiner(plural, det(those)) --> [those].
determiner(_, _) --> [].

pronoun(singular, masculine, he) --> [he].
pronoun(singular, feminine, she) --> [she].
pronoun(singular, neutral, that) --> [that].
pronoun(plural, neutral, those) --> [those].
pronoun(singular, neutral, Pro) --> [Pro], {member(Pro, [i, someone, it])}.
pronoun(plural, neutral, Pro) --> [Pro], {member(Pro, [they, some])}.

possessive_pronoun(singular, masculine, his) --> [his].
possessive_pronoun(singular, feminine, her) --> [her].

prep(of) --> [of].
prep(to) --> [to].
prep(with) --> [with].
prep(in) --> [in].
prep(for) --> [for].

num(singular) --> [one].
num(plural) --> [two];[three];[four];[five];[six];[seven];[eight];[nine];[ten].

noun(singular, Noun) --> [Noun], {thing(Noun, Props), member(number(singular), Props)}.
noun(plural, Noun) --> [Noun], {thing(Noun, Props), member(number(plural), Props)}.

proper_noun(singular, Gender, Name) -->
	[Name],
	{
		thing(Name, Props), member(isa(person), Props), member(gender(Gender), Props)
	}.
proper_noun(singular, neutral, france) --> [france].

adjective(prop(Adj)) --> [Adj], {member(Adj, [red,green,blue])}.

verb(_, Verb) --> [Verb], {member(Verb, [lost,found,did,gave,looked,saw,forgot,is])}.
verb(singular, Verb) --> [Verb], {member(Verb, [scares,hates])}.
verb(plural, Verb) --> [Verb], {member(Verb, [scare,hate])}.

adverb([adv(too)]) --> [too].
adverb([]) --> [].

% You may chose to use these items in the database to provide another way
% of capturing an objects properties.

thing(john, [isa(person), gender(masculine), number(singular)]).
thing(sam, [isa(person), gender(masculine), number(singular)]).
thing(bill, [isa(person), gender(masculine), number(singular)]).
thing(jack, [isa(person), gender(masculine), number(singular)]).
thing(monet, [isa(person), gender(masculine), number(singular)]).

thing(mary, [isa(person), gender(feminine), number(singular)]).
thing(annie, [isa(person), gender(feminine), number(singular)]).
thing(sue, [isa(person), gender(feminine), number(singular)]).
thing(jill, [isa(person), gender(feminine), number(singular)]).

thing(wallet, [isa(physical_object), gender(neutral), number(singular)]).
thing(car, [isa(physical_object), gender(neutral), number(singular)]).
thing(book, [isa(physical_object), gender(neutral), number(singular)]).
thing(telescope, [isa(physical_object), gender(neutral), number(singular)]).
thing(pen, [isa(physical_object), gender(neutral), number(singular)]).
thing(pencil, [isa(physical_object), gender(neutral), number(singular)]).
thing(cat, [isa(physical_object), gender(neutral), number(singular)]).
thing(mouse, [isa(physical_object), gender(neutral), number(singular)]).
thing(man, [isa(physical_object), gender(neutral), number(singular)]).
thing(bear, [isa(physical_object), gender(neutral), number(singular)]).

thing(cats, [isa(physical_object), gender(neutral), number(plural)]).
thing(mice, [isa(physical_object), gender(neutral), number(plural)]).
thing(men, [isa(physical_object), gender(neutral), number(plural)]).
thing(bears, [isa(physical_object), gender(neutral), number(plural)]).

thing(capital, [isa(abstract_object), gender(neutral), number(singular)]).

thing(france, [isa(place), gender(neutral), number(singular)]).

event(lost, [actor(_), object(_), tense(past)]).
event(found, [actor(_), object(_), tense(past)]).
event(saw, [actor(_), object(_), tense(past)]).
event(forgot, [actor(_), object(_), tense(past)]).
event(scares, [actor(_), object(_), tense(present), number(singular)]).
event(scare, [actor(_), object(_), tense(present), number(plural)]).
event(hates, [actor(_), object(_), tense(present), number(singular)]).
event(hate, [actor(_), object(_), tense(present), number(plural)]).
event(gave, [actor(Person1), recipient(Person2), object(_), tense(past)]) :- Person1 \= Person2.

personal(i, [number(singular), gender(neutral)]).
personal(he, [number(singular), gender(masculine)]).
personal(she, [number(singular), gender(feminine)]).
personal(it, [number(singular), gender(neutral)]).
personal(that, [number(singular), gender(neutral)]).
personal(those, [number(plural), gender(neutral)]).
personal(they, [number(plural), gender(neutral)]).

possessive(his, [number(singular), gender(masculine)]).
possessive(her, [number(singular), gender(feminine)]).


% Process events and store them in history
process(event(Verb, NPs), [], Refs) :-
    process(NPs, [], Refs),
    assert(history(event(Verb, NPs))).

% Recursively go through each item in the event list
process([NP1 | Rest], [], Refs) :-
    process(NP1, [], Ref1),
    process(Rest, [], Ref2),
    merge_list(Ref1, Ref2, Refs).


% Process actor item
process(actor(NPs), [], Refs) :- 
    process(NPs, [], Refs).


% Process object item
process(object(NPs), [], Refs) :-
    process(NPs, [], Refs).


% Process instrument item
process(instrument(NPs), [], Refs) :-
    process(NPs, [], Refs).


% Process a set by going each object in the set
% 2-item set
process(set(NP1, NP2), [], Refs) :-
% process(set(NP1, NP2), [], Refs) :-
    process(NP1, [], Ref1),
    process(NP2, [], Ref2),
    merge_list(Ref1, Ref2, Refs),
    assert(history(set(NP1, NP2))).

% 3-item set
process(set(NP1, NP2, NP3), [], Refs) :-
% process(set(NP1, NP2, NP3), [], Refs) :-
    process(NP1, [], Ref1),
    process(NP2, [], Ref2),
    process(NP3, [], Ref3),
    merge_list(Ref1, Ref2, Temp),
    merge_list(Temp, Ref3, Refs),
    assert(history(set(NP1, NP2, NP3))).


% Process thing item
% Thing without adj, adv or propositional phrase
process(thing(Noun, []), [], []) :-
    % Get the properties of the noun
    thing(Noun, Props),
    % Store it in the properties
    assert(history(thing(Noun, Props))).

% Thing with adj, adv or propositional phrase
process(thing(Noun, PP), [], Refs) :-
    % Get the properties of the noun
    thing(Noun, Props),
    assert(history(thing(Noun, Props))),
    % Proccess the additional phrase
    process(PP, [], Refs).
process(thing(_, _), [], []).


% Process personal pronoun
% Find reference to singular pronoun (he/she/it) or plural neutral word (cats, mice...) 
process(personal(PP), [], [LatestThing]) :-
    % Get number and gender properties of a pronoun
    personal(PP, [Number, Gender]),
    % Return a list of things with same properties from history
    find_things_history([Gender, Number], Nouns),
    % Prioritise the latest thing stored
    last(Nouns, LatestThing).

% Find reference to plural pronoun as the latest set stored in history
% 2-item set
process(personal(_), [], [Refs]) :-
    % Get all the sets in history
    findall([NP1, NP2], history(set(NP1, NP2)), Lists),
    % Prioritise the latest set
    last(Lists, LatestList),
    % Process these noun phrases
    find_things_in_list(LatestList, Refs).

% 3-item set
process(personal(_), [], [Refs]) :-
    % Get all the sets in history
    findall([NP1, NP2, NP3], history(set(NP1, NP2, NP3)), Lists),
    % Prioritise the latest set
    last(Lists, LatestList),
    % Process these noun phrases
    find_things_in_list(LatestList, Refs).

% Find reference to plural pronoun as 2 singular things (1 masculine and 1 feminine)
process(personal(_), [], [Noun1, Noun2]) :-
    % Get list of latest singular, masculine person from history
    find_things_history([gender(masculine), number(singular)], Nouns1),
    % Prioritise the latest in the list
    last(Nouns1, Noun1),
    % Get list of latest singular, feminine person from history
    find_things_history([gender(feminine), number(singular)], Nouns2),
    % Prioritise the latest in the list
    last(Nouns2, Noun2).

% Find reference to plural pronoun as 2 singular masculine person from history
process(personal(_), [], [Noun1, Noun2]) :-
    % Get list of latest singular, masculine person from history
    find_things_history([isa(person), gender(masculine), number(singular)], Nouns),
    % Prioritise the latest two in the list
    last_two(Nouns, [Noun1, Noun2]),
    % Make sure they are not the same person
    Noun1 \== Noun2.

% Find reference to plural pronoun as 2 singular feminine person from history
process(personal(_), [], [Noun1, Noun2]) :-
    % Get list of latest singular, feminine person from history
    find_things_history([isa(person), gender(feminine), number(singular)], Nouns),
    last_two(Nouns, [Noun1, Noun2]),
    % Make sure they are not the same person
    Noun1 \== Noun2.

% Find reference to plural pronoun as two singular neutral things 
% (physical_object, abstract_object, place) from history
process(personal(they), [], Refs) :-
    find_things_history([gender(neutral), number(singular)], Nouns),
    last_two(Nouns, Refs).


% Process possessive pronoun
process(possessive(PP, Noun), [], Refs) :-
    % Get properties of the pronoun
    possessive(PP, [Number, Gender]),
    % Return a list of things with same properties from history
    find_things_history([Gender, Number], Nouns),
    % Select the latest one
    last(Nouns, LatestThing),
    % Process the noun following it
    process(Noun, [], Ref1),
    % Put references together
    merge_list([LatestThing], Ref1, Refs).


% Process noun
process(Noun, [], []) :-
    thing(Noun, Props),
    assert(history(thing(Noun, Props))).


% Process propositional phrase, adjective, adverb
process(prop(_), [], []).
process(adj(_), [], []).
process(adv(_), [], []).
process([], [], []).


% Help functions
% Return the list of noun/name from history that match given properties
find_things_history([Gender, Number], Nouns) :-
    findall(Noun, history(thing(Noun, [_, Gender, Number | _])), Nouns).
find_things_history([Isa, Gender, Number], Nouns) :-
    findall(Noun, history(thing(Noun, [Isa, Gender, Number | _])), Nouns).

% Return the noun/name from the given thing()/possessive()
find_thing(thing(Noun, _), [Noun]).
find_thing(possessive(_, NP), Noun) :-
    find_thing(NP, Noun).
% Return the list of nouns/names from given list
find_things_in_list([], []).
find_things_in_list([NP1 | Rest], Refs) :-
    find_thing(NP1, Ref1),
    find_things_in_list(Rest, Ref2),
    merge_list(Ref1, Ref2, Refs).

% Merge two given lists
merge_list([], [], []).
merge_list(X, [], X).
merge_list([], X, X).
merge_list([X | L1], L2, [X | L]):-
    merge_list(L1, L2, L).

% Get the last item in given list
last([X], X).
last([_ | X], Y) :-
    last(X, Y).

% Get the last two items in given list
last_two([X, Y], [X, Y]).
last_two([_ | X], Y) :-
    last_two(X, Y).


run(S, Refs) :-
	sentence(X, S, []), !,
	writeln(X),
	process(X, [], Refs),
	listing(history/1).
