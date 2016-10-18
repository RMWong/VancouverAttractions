/* CPSC312: VancouverAttractions */

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% Determiners are ignored in this oversimplified example.
det([the | T],T,_,C,C).
det(T,T,_,C,C).

% adjectives consist of a sequence of adjectives.
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adjective(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).
	
adjective([Lang,speaking | T],T,Obj,C,[language(Obj,Lang)|C]).

% An optional modifying phrase / relative clause is either
% nothing or
% a prepositional phrase or
% that followed by a verb phrase
mp(T,T,_,C,C).
mp(T0,T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).

reln([the,attractions,in | T],T,O1,O2,C,[attraction(O2,O1)|C]).
reln([the,cost,of | T],T,O1,O2,C,[cost(O2,O1)|C]).
reln([the,location,of | T],T,O1,O2,C,[location(O2,O1)|C]).
reln([the,description,of | T],T,O1,O2,C,[description(O2,O1)|C]).

% question(Question,QR,Object,Q0,Query) is true if Query provides an answer about Object to Question
question([what,are | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([what,are | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([what,is | T0],T1,Obj,C0,C1) :-
	mp(T0,T1,Obj,C0,C1).
	
noun([vancouver | T],T,vancouver,C,C).
noun([scienceworld | T],T,scienceworld,C,C).
noun([gastown | T],T,gastown,C,C).
noun([lynncanyon | T],T,lynncanyon,C,C).

% ask(Q,A) gives answer A to question Q relating to attractions in Vancouver
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).
	
% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

%  The Database of Facts to be Queried
attraction(vancouver, scienceworld).
attraction(vancouver, gastown).
attraction(vancouver, lynncanyon).

cost(scienceworld, 19.40).
cost(gastown, 0).
cost(lynncanyon, 0).

location(scienceworld, [1455,quebec,street]).
location(gastown, [300,water,street]).
location(lynncanyon, [north,vancouver]).

description(scienceworld, [science,world,is,a,charitable,organization,that,engages,british,columbians,in,science,and,inspires,future,science,and,technology,leadership,throughout,our,province]).
description(gastown,[vancouvers,historic,heart,and,epicenter,of,indepedent,design,culture,food,and,fashion]).
description(lynncanyon,[great,place,for,picnic,and,park,trail,with,suspension,bridge]).




/* Basic queries:
?- ask([what,are,the,attractions,in,vancouver],A).  // returns all attraction names

?- ask([what,is,the,cost,of,X],A).			// returns cost of specified attraction name X
?- ask([what,is,the,location,of,X],A).			// returns location of specified attraction name X in form of a list
?- ask([what,is,the,description,of,X],A).		// returns description of specified attraction name X in form of list
*/
