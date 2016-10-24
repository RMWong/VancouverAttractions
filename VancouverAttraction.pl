/* CPSC312: VancouverAttractions */
:- dynamic rating/2.
:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), spacing(next_argument)]).

% Extra part. Include ratings
rate(A,R) :-
  write('What rating do you give '),
  write(A),
  write('?'), nl,
  read(R),
  write('You gave '), write(A), write(' a rating of '), write(R), assert(rating(A,R)).

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% Determiners are ignored in this oversimplified example.
det([the | T],T,_,C,C).
det([an | T],T,_,C,C).
det(T,T,_,C,C).

% adjectives consist of a sequence of adjectives.
adjectives(T,T,_,C,C).

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
reln([the,attractions,that,have,rating,P | T],T,O1,_,C,[rating(O1,P)|C]).
reln([an,attraction,that,costs,C1 | T],T,O1,_,C,[cost(C1,O1)|C]).
reln([the,cost,of | T],T,O1,O2,C,[cost(O2,O1)|C]).
reln([the,location,of | T],T,O1,O2,C,[location(O2,O1)|C]).
reln([the,description,of | T],T,O1,O2,C,[description(O2,O1)|C]).
reln([the,rating,for | T],T,O1,O2,C,[rating(O2,O1)|C]).
reln([the,ratings,for | T],T,O1,O2,C,[rating(O2,O1)|C]).

% question(Question,QR,Object,Q0,Query) is true if Query provides an answer about Object to Question
question([what,are | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([what,are | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([what,is | T0],T1,Obj,C0,C1) :-
  mp(T0,T1,Obj,C0,C1).

noun([vancouver | T],T,vancouver,C,C).
noun([dollars | T],T,_,C,C).
noun([points | T],T,_,C,C).
noun([scienceworld | T],T,scienceworld,C,C).
noun([gastown | T],T,gastown,C,C).
noun([lynncanyon | T],T,lynncanyon,C,C).
noun([stanleyPark | T],T,stanleyPark,C,C).
noun([queensPark | T],T,queensPark,C,C).
noun([capilanoSuspensionBridge | T],T,capilanoSuspensionBridge,C,C).
noun([bloedelFloralConservatory | T],T,bloedelFloralConservatory,C,C).
noun([vancouverAquarium | T],T,vancouverAquarium,C,C).
noun([vanDuesenBotanicalGarden | T],T,vanDuesenBotanicalGarden,C,C).
noun([beatyBiodiversityMuseum | T],T,beatyBiodiversityMuseum,C,C).
noun([museumOfAnthropology | T],T,museumOfAnthropology,C,C).
noun(["h.R.MacMillanSpaceCentre" | T],T,"h.R.MacMillanSpaceCentre",C,C).
noun([spanishBanks | T],T,spanishBanks,C,C).
noun([kitsilanoBeach | T],T,kitsilanoBeach,C,C).
noun([bcPlace | T],T,bcPlace,C,C).
noun([rogersArena | T],T,rogersArena,C,C).
noun([granvilleIsland | T],T,granvilleIsland,C,C).

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
attraction(vancouver, stanleyPark).
attraction(vancouver, vancouverAquarium).
attraction(vancouver, vanDuesenBotanicalGarden).
attraction(vancouver, queensPark).
attraction(vancouver, bloedelFloralConservatory).
attraction(vancouver, rodgersArena).
attraction(northVancouver, capilanoSuspensionBridge).
attraction(vancouver, granvilleIsland).
attraction(vancouver, museumOfAnthropology).
attraction(vancouver, bcPlace).
attraction(vancouver, "h.R.MacMillanSpaceCentre").
attraction(vancouver, kitsilanoBeach).
attraction(vancouver, spanishBanks).
attraction(vancouver, beatyBiodiversityMuseum).
attraction(vancouver, scienceworld).
attraction(vancouver, gastown).
attraction(northVancouver, lynnCanyon).

tags(stanleyPark, park).
tags(lynnCanyon, park).
tags(queensPark, park).
tags(capilanoSuspensionBridge, park).
tags(bloedelFloralConservatory, park).
tags(scienceworld, museum).
tags(vancouverAquarium, museum).
tags(vanDuesenBotanicalGarden, park).
tags(beatyBiodiversityMuseum, museum).
tags(museumOfAnthropology, museum).
tags("h.R.MacMillanSpaceCentre", museum).
tags(spanishBanks, beach).
tags(kitsilanoBeach, beach).
tags(bcPlace, stadium).
tags(rodgersArena, stadium).
tags(granvilleIsland, area).
tags(gastown, area).

cost(0, stanleyPark).
cost(stanleyPark, 0).
cost(0,lynnCanyon).
cost(lynnCanyon, 0).
cost(0,queensPark).
cost(queensPark, 0).
cost(19.40, scienceWorld).
cost(scienceWorld, 19.40).
cost(capilanoSuspensionBridge, ["adult(17-64)",39.95,"senior(65+)",36.95,student,32.95,"youth(13-16)",26.95,"child(6-12)",13.95,kids,free]).
cost(bloedelFloralConservatory, ["adult(19-64)",6.75,"senior(65+)",4.50,"youth(13-18)",4.50,"child(3-12)",3.25,"infant(0-2)",free]).
cost(vancouverAquarium, ["adult(19-64)",36,"senior(65+)",27,"youth(13-18)",27,student,27,"child(4-12)",21,"infant(0-3)",free]).
cost(vanDuesenBotanicalGarden, ["adult(19-64)",11.25,"senior(65+)",8.50,"youth(13-18)",8.50,"child(3-12)",5.75,"infant(0-2)",free]).
cost(beatyBiodiversityMuseum, [adult,14,"senior(55+)",12,"youth(13-17)",12,"child(5-12)",10,"child(0-4)",free]).
cost(museumOfAnthropology, [adults,18,students/seniors,16,"family(2adults,4children)",47]).
cost("h.R.MacMillanSpaceCentre", ["adult(19-54)",18.00,"youth(12-18)","senior(55+)",15.00,"child(5-11)",13.00,"child(0-5)",free]).
cost(spanishBanks, 0).
cost(kitsilanoBeach, 0).
cost(bcPlace, varied).
cost(rodgersArena, varied).
cost(granvilleIsland, 0).
cost(gastown, 0).

%% Find prices of all relevant things

location(stanleyPark, [vancouver,bc,v6g1z4]).
location(lynnCanyon, [3663,park,rd]).
location(queensPark, [4600,cambie,st]).
location(capilanoSuspensionBridge, [3735,capilano,rd]).
location(bloedelFloralConservatory, [5251,oak,st]).
location(scienceworld, [1455,quebec,street]).
location(vancouverAquarium, [845,avison,way]).
location(vanDuesenBotanicalGarden, [5251,oak,st]).
location(beatyBiodiversityMuseum, [2212,main,mall]).
location(museumOfAnthropology, [6393,nw,marine,dr]).
location("h.R.MacMillanSpaceCentre", [1100,chestnut,st]).
location(spanishBanks, [english,bay]).
location(bcPlace, [777,pacific,blvd]).
location(rogersArena, [800,griffiths,way]).
location(granvilleIsland, [1669,johnston,st]).
location(gastown, [300,water,st]).


rating(stanleyPark,0).
rating(lynnCanyon,0).
rating(queensPark,0).
rating(capilanoSuspensionBridge,0).
rating(bloedelFloralConservatory,0).
rating(scienceworld,0).
rating(vanDuesenBotanicalGarden,0).
rating(museumOfAnthropology,0).
rating("h.R.MacMillanSpaceCentre",0).
rating(spanishBanks,0).
rating(bcPlace,0).
rating(spanishBanks,0).
rating(rogersArena,0).
rating(granvilleIsland,0).
rating(gastown,0).
rating(vancouverAquarium,0).

description(stanleyPark, [a,405,hectare,public,park,bordering,downtown,vancouver]).
description(lynnCanyon,[great,place,for,picnic,and,park,trail,with,suspension,bridge, and,hikes]).
description(queensPark, [130,acre,municipal,park,on,little,mountain]).
description(capilanoSuspensionBridge, [a,popular,tourist,attraction,and,a,suspension,bridge,crossing,the,capilano,river]).
description(bloedelFloralConservatory, [bird,conservatory,located,in,the,vandusen,gardens]).
description(scienceworld, [science,world,is,a,charitable,organization,that,engages,british,columbians,in,science,and,inspires,future,science,and,technology,leadership,throughout,our,province]).
description(vancouverAquarium, [a,public,aquarium,located,in,stanley,park]).
description(vanDuesenBotanicalGarden, [botanical,garden,loceted,in,central,vancouver]).
description(beatyBiodiversityMuseum, [the,beaty,biodiversity,museum,is,a,natural,history,museum,on,the,campus,of,the,university,of,british,columbia]).
description(museumOfAnthropology, [a,museum,renowned,for,its,displays,of,world,art,and,culture,in,particular,works,by,first,nations]).
description("h.R.MacMillanSpaceCentre", [an,astronomy,museum,founded,in,1968]).
description(spanishBanks, [spanish,banks,are,a,series,of,beaches,on,the,shores,of,english,bay]).
description(bcPlace, [multi,purpose,stadium,located,at,the,north,side,of,false,creek]).
description(rogersArena, [indoor,sports,arena,located,downtown]).
description(granvilleIsland, [granville,island,is,a,peninsula,and,shopping,district]).
description(gastown,[vancouvers,historic,heart,and,epicenter,of,indepedent,design,culture,food,and,fashion]).

howLongAt(stanleyPark, [1,-,3,hours]).
howLongAt(lynnCanyon, [1,-,3,hours]).
howLongAt(queensPark, [1,-,2,hours]).
howLongAt(capilanoSuspensionBridge, [1,-,2,hours]).
howLongAt(bloedelFloralConservatory, [1,-,2,hours]).
howLongAt(scienceworld, [2,-,4,hours]).
howLongAt(vancouverAquarium, [2,-,3,hours]).
howLongAt(vanDuesenBotanicalGarden, [2,-,3,hours]).
howLongAt(beatyBiodiversityMuseum, [1,-,2,hours]).
howLongAt(museumOfAnthropology, [2,-,3,hours]).
howLongAt("h.R.MacMillanSpaceCentre", [2,-,3,hours]).
howLongAt(spanishBanks, [1,-,4,hours]).
howLongAt(bcPlace, [2,-,4,hours]).
howLongAt(rogersArena, [2,-,4,hours]).
howLongAt(granvilleIsland, [1,-,2,hours]).
howLongAt(gastown, [1,-,2,hours]).


/* Basic queries:
?- ask([what,are,the,attractions,in,vancouver],A).  // returns all attraction names
?- ask([what,are,the,attractions,that,have,rating,X,points],A).
?- ask([what,is,an,attraction,that,costs,19.40,dollars],A).
?- ask([what,is,the,cost,of,X],A).      // returns cost of specified attraction name X
?- ask([what,is,the,location,of,X],A).      // returns location of specified attraction name X in form of a list
?- ask([what,is,the,description,of,X],A).   // returns description of specified attraction name X in form of list
*/

/* Extra part
    rate(scienceworld, A). // Will prompt a user to enter a numerical rating for the attraction
    The rating can dynamically be read using
    ask([what,is,the,rating,for,X],A).
    OR
    ask([what,are,the,ratings,for,X],A).
*/