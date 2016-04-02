:- dynamic(date/3).
:- dynamic(bye/2).
:- dynamic(divMatch/2).

standings(afc,east,1,patriots).
standings(afc,east,2,jets).
standings(afc,east,3,bills).
standings(afc,east,4,dolphins).

standings(afc,north,1,bengals).
standings(afc,north,2,steelers).
standings(afc,north,3,ravens).
standings(afc,north,4,browns).

standings(afc,south,1,texans).
standings(afc,south,2,colts).
standings(afc,south,3,jaguars).
standings(afc,south,4,titans).

standings(afc,west,1,broncos).
standings(afc,west,2,chiefs).
standings(afc,west,3,raiders).
standings(afc,west,4,chargers).

standings(nfc,east,1,redskins).
standings(nfc,east,2,eagles).
standings(nfc,east,3,giants).
standings(nfc,east,4,cowboys).

standings(nfc,north,1,vikings).
standings(nfc,north,2,packers).
standings(nfc,north,3,lions).
standings(nfc,north,4,bears).

standings(nfc,south,1,panthers).
standings(nfc,south,2,falcons).
standings(nfc,south,3,saints).
standings(nfc,south,4,buccaneers).

standings(nfc,west,1,cardinals).
standings(nfc,west,2,seahawks).
standings(nfc,west,3,rams).
standings(nfc,west,4,s49ers).


intra(north,east).
intra(east,north).
intra(south,west).
intra(west,south).

inter(east,west).
inter(north,east).
inter(south,north).
inter(west,south).

findRivInter(afc,X,Y) :- inter(X,Y).
findRivInter(nfc,X,Y) :- inter(Y,X).

teams(X) :- findall(Team,standings(_,_,_,Team),X).

% Arreglar esto.
byes(Z) :- teams(X), byes(X, [], Z).
byes([], Z, Z).
byes(X, ACC, Z):- selectn(4,A,X,B), byes(B, [A | ACC], Z).

% Recordar dar información de donde se sacó.
selectn(0, [], Rest, Rest).
selectn(N, [A|B], C, Rest) :-
    append(H, [A|T], C),
    M is N-1,
    selectn(M, B, T, S),
    append(H, S, Rest).


rivals(Team,LR) :- standings(C,D,P,Team),
				findall(Rival1,(standings(C,D,_,Rival1),Rival1\==Team),Div),
				divisionMatches(Team,Div,DivMatches),
				intra(D,IntraDiv), findall(Rival2,(standings(C,IntraDiv,_,Rival2)),Intra),
				intraInterMatches(Team,Intra,IntraMatches),
				append(DivMatches,IntraMatches,A),
				findRivInter(C,D,InterDiv),
				findall(Rival3,
						standings(C,InterDiv,_,Rival3),Inter
				),
				intraInterMatches(Team,Inter,InterMatches),
        		append(A,InterMatches,B),
				findall(Rival4,(standings(C,PDiv,P,Rival4),PDiv \== D,PDiv \== IntraDiv),SamePos),
				samePosMatches(Team,SamePos,PosMatches),
        		append(B,PosMatches,LR).




divisionMatches(Team,Teams,Matches) :- divisionMatches(Team,Teams,Matches,[]).
divisionMatches(T,[],M,M).
divisionMatches(T,[X|XS],M,ACC) :- divisionMatches(T,XS,M,[(T,X),(X,T)|ACC]).

intraInterMatches(Team,Teams,Matches) :- selectn(2,[R1,R2],Teams,A),
										 selectn(2,[R3,R4],A,Y),
										 Matches = [(Team,R1),(Team,R2),(R3,Team),(R4,Team)].

samePosMatches(Team,Teams,Matches) :- select(R1,Teams,R),
									  select(R2,R,Y),
									  Matches = [(Team,R1),(R2,Team)].




genByes([],_).
genByes([X|Xs],N) :- asserta(bye(N,X)), N1 is N + 1, genByes(Xs,N1).

printDates(18) :- !.
printDates(N) :- write('Week '),write(N),nl,
			write('---------'),nl,
			findall((V,L),date(N,V,L),Ms),
			printTeams(Ms), nl,
			printBye(N),nl,
			M is N + 1,
			printDates(M).

printBye(N) :- N < 9, bye(N,Bye), write('Bye: '), write(Bye), nl, nl.
printBye(N).

printTeams([]).
printTeams([(V,L)|Xs]) :- write(V), write(' at '), write(L),nl,printTeams(Xs).

genWeeks([]).
genWeeks(Teams):-
	select(T1,Teams,Rest),
	asserta(divMatch(T1,3)),
	rivals(T1,L),
	addDate(17,L),
	genWeeks(Rest).
				
addDate(0,_).
addDate(N,R) :- between(14,17,N),
				select((E1,E2),R,Rest),
				\+date(N,E1,_), \+date(N,_,E1), \+date(N,E2,_), \+date(N,_,E2),
				verifyDivMatch(E1,E2),
				asserta(date(N,E1,E2)),
				M is N - 1,
				addDate(M,Rest).
addDate(N,R) :- select((E1,E2),R,Rest),
				\+date(N,E1,_), \+date(N,_,E1), \+date(N,E2,_), \+date(N,_,E2),
				verifyByes(N,E1,E2),
				asserta(date(N,E1,E2)),
				M is N - 1,
				addDate(M,Rest).
addDate(N,R) :- M is N - 1,
				addDate(M,R).

verifyByes(N,E1,E2) :- N < 9, bye(N,Bye), \+member(E1,Bye), \+member(E2,Bye).
verifyByes(N,E1,E2) :- N > 8.

verifyDivMatch(E1,E2):- divMatch(E1,0), divMatch(E2,0).
verifyDivMatch(E1,E2):- standings(C,D,_,E1), standings(C,D,_,E2),
						divMatch(E1,N1), M1 is N1 - 1,
						divMatch(E2,N2), M2 is N2 - 1,
						asserta(divMatch(E1,M1)),
						asserta(divMatch(E2,M2)).

schedule :- findall(T,standings(_,_,_,T),Teams),
			byes(Z),
			genByes(Z,1),
			genWeeks(Teams),
			printDates(1),
			retractall(date(_,_,_)).
