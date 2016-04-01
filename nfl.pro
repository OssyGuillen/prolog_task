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
				(C == afc -> 
					inter(D,InterDiv),
					findall(Rival3,
							standings(nfc,InterDiv,_,Rival3),Inter
							);
					inter(InterDiv,D),
					findall(Rival3,
							standings(afc,InterDiv,_,Rival3),Inter
							)
				),
				intraInterMatches(Team,Inter,InterMatches),
				findall(Rival4,(standings(C,PDiv,P,Rival4),PDiv \== D,PDiv \== IntraDiv),SamePos),
				samePosMatches(Team,SamePos,PosMatches),
				append(DivMatches,IntraMatches,A),
        		append(A,InterMatches,B),
        		append(B,PosMatches,LR), !.




divisionMatches(Team,Teams,Matches) :- divisionMatches(Team,Teams,Matches,[]).
divisionMatches(T,[],M,M).
divisionMatches(T,[X|XS],M,ACC) :- divisionMatches(T,XS,M,[(T,X),(X,T)|ACC]).

intraInterMatches(Team,Teams,Matches) :- intraInterMatches(Team,Teams,Matches,[]).
intraInterMatches(T,[],M,M).
intraInterMatches(T,[R1,R2],Matches,ACC) :- intraInterMatches(T,[],Matches,[(R1,T),(R2,T)|ACC]).
intraInterMatches(T,[R1,R2|XS],Matches,ACC) :- intraInterMatches(T,XS,Matches,[(T,R1),(T,R2)|ACC]).

samePosMatches(Team,[R1,R2],[(Team,R1),(R2,Team)]).

