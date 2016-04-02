% Universidad Simón Bolívar
% Departamento de Computación y Tecnología de la Información
% CI3661 - Laboratorio de Lenguajes de Programación I
% Trimestre Enero - Marzo 2016.
% 
% Tarea Prolog.
%
% Calendario NFL.
%  
% Autores:          Gabriel Iglesias 11-10476.
%                   Oscar Guillen    11-11264.
% Grupo:     		G16
% Última edición: 	01 de abril de 2016.

% Declaración de predicados dinámicos
% date - indica un partido para una semana específica.
% bye - indica la lista de byes para una semana específica.
% divMatch - indica la cantidad de partidos divisionales
% 	restantes para completar la especificación de jugar
% 	los últimos tres en las las semanas 14-15-16-17.
:- dynamic(date/3).
:- dynamic(bye/2).
:- dynamic(divMatch/2).

% Declaración de los resultados de 
% la última temporada de la NFL.

% AFC
standings(afc,north,1,bengals).
standings(afc,north,2,steelers).
standings(afc,north,3,ravens).
standings(afc,north,4,browns).

standings(afc,south,1,texans).
standings(afc,south,2,colts).
standings(afc,south,3,jaguars).
standings(afc,south,4,titans).

standings(afc,east,1,patriots).
standings(afc,east,2,jets).
standings(afc,east,3,bills).
standings(afc,east,4,dolphins).

standings(afc,west,1,broncos).
standings(afc,west,2,chiefs).
standings(afc,west,3,raiders).
standings(afc,west,4,chargers).

% NFC
standings(nfc,north,1,vikings).
standings(nfc,north,2,packers).
standings(nfc,north,3,lions).
standings(nfc,north,4,bears).

standings(nfc,south,1,panthers).
standings(nfc,south,2,falcons).
standings(nfc,south,3,saints).
standings(nfc,south,4,buccaneers).

standings(nfc,east,1,redskins).
standings(nfc,east,2,eagles).
standings(nfc,east,3,giants).
standings(nfc,east,4,cowboys).

standings(nfc,west,1,cardinals).
standings(nfc,west,2,seahawks).
standings(nfc,west,3,rams).
standings(nfc,west,4,s49ers).


% Declaración de emparejamientos intra-divisionales.
intra(north,east).
intra(east,north).
intra(south,west).
intra(west,south).

% Declaración de emparejamientos inter-divisionales.
inter(east,west).
inter(north,east).
inter(south,north).
inter(west,south).

%% schedule
%
%  Calcula y muestra los calendarios balanceados para la temporada
% 2016 de la NFL, siguiendo los criterios del proyecto.

schedule :- 
	% Se buscan todos los equipos de la NFL.
	findall(T,standings(_,_,_,T),Teams),

	% Se generan los byes para el calendario.
	byes(Z),

	% Se generan los predicados dinámicos para los byes.
	genByes(Z,1),

	% Se generan los partidos para todas las semanas.
	genWeeks(Teams),

	% Se imprimen los resultados.
	printDates(1),

	% Se borran los partidos por fechas para generarlos nuevamente.
	retractall(date(_,_,_)).


%% byes (?Z: list) 
%
% Genera las listas de 8 sublistas de 4 elementos que indican los
% equipos libres para una jornada o semana.
%
% @param Z  Lista generada.

byes(Z) :- findall(Team,standings(_,_,_,Team),X), byes(X, [], Z).
byes([], Z, Z).
byes(X, ACC, Z):- selectn(4,A,X,B), byes(B, [A | ACC], Z).


%% selectn (+N: int, ?A: list, +L: list, ?R: list) 
%
% Toma todas las posibles N elementos de una lista L, retornando
% la lista A con los elementos seleccionados y una lista R con el
% resto de los elementos.
%
% @param N Cantidad de elementos a seleccionar.
% @param A Lista de elementos seleccionados.
% @param L Lista de la cual tomar los elementos.
% @param R Lista de los elementos restantes.
%
% Nota: Ésta función es reutilizada de la idea de: CapelliC
% Disponible en: stackoverflow.com/questions/17400598/select-n-elements-from-a-list-in-prolog
% Última consulta: 01/04/2016.

selectn(0, [], Rest, Rest).
selectn(N, [A|B], C, Rest) :-
    append(H, [A|T], C),
    M is N-1,
    selectn(M, B, T, S),
    append(H, S, Rest).


%% genByes (+X: list, +N: int) 
%
% Genera los predicados dinámicos bye/2, a partir de la lista
% de byes X tomada, asignando desde la semana N.
%
% @param X  Lista de byes.
% @param N 	Auxiliar para llevar el número de la semana.

genByes([],_).
genByes([X|Xs],N) :- 
	asserta(bye(N,X)), N1 is N + 1, genByes(Xs,N1).


%% genWeeks (+T: list) 
%
% Genera todos los partidos por semana para cada equipo.
%
% @param T  Lista de equipos.

genWeeks([]).
genWeeks(Teams):-
	% Se selecciona un equipo.
	select(T1,Teams,Rest),

	% Se agrega el predicado dinámico para los partidos divisionales a
	% final de temporada.
	asserta(divMatch(T1,3)),

	% Se generan todos los partidos para el equipo.
	rivals(T1,L),

	% Se agregan los partidos al calendario(como predicados dinámicos).
	addDate(17,L),

	% Se generan los partidos para el resto de equipos.
	genWeeks(Rest).


%% rivals (+T , ?L: list) 
%
% Genera los partidos con todos los rivales para el equipo de T
% retornándolos como duplas en la lista L. 
%
% @param T  Equipo a buscar encuentros.
% @param L  Lista de partidos para el equipo T.

rivals(Team,LR) :- 
	% Se busca la conferencia, división y posición del equipo.
	standings(C,D,P,Team),

	% Se buscan todos los rivales de la misma división y conferencia
	% y se generan todos los posibles partidos.
	findall(
		Rival1,
		(standings(C,D,_,Rival1),Rival1\==Team),
		Div
	),
	divisionMatches(Team,Div,DivMatches),

	% Se buscan todos los rivales intra-divisionales y se generan todos
	% los posibles partidos.
	intra(D,IntraDiv), 
	findall(
		Rival2,
		(standings(C,IntraDiv,_,Rival2)),
		Intra
	),
	intraInterMatches(Team,Intra,IntraMatches),

	% Se unen las listas de partidos ya generados.
	append(DivMatches,IntraMatches,A),

	% Se buscan los rivales intra-divisionales a partid de la conferencia y 
	% se generan todos los posibles partidos.
	findRivInter(C,D,InterDiv),
	findall(
		Rival3,
		standings(C,InterDiv,_,Rival3),
		Inter
	),
	intraInterMatches(Team,Inter,InterMatches),

	% Se unen las listas de partidos.
	append(A,InterMatches,B),

	% Se buscan los rivales en las mismas posiciones del equipo
	% pero que no esten en su misma división, ni intra, ni inter.
	% Se generan los posibles partidos.
	findall(
		Rival4,
		(standings(C,PDiv,P,Rival4),PDiv \== D,PDiv \== IntraDiv),
		SamePos
	),
	samePosMatches(Team,SamePos,PosMatches),

	% Se obtiene la lista final de partidos.
	append(B,PosMatches,LR).



%% divisionMatches (+T, +Ts: list, ?Ms: list) 
%
% Genera todos los posibles partidos divisionales para un equipo T,
% y los rivales en Ts, pertenencientes a la misma división y conferen-
% cia en la lista Ms.
%
% @param T  Equipo.
% @param Ts Lista de rivales del equipo T.
% @param Ms  Lista de partidos generados.

divisionMatches(Team,Teams,Matches) :- 
	divisionMatches(Team,Teams,Matches,[]).
divisionMatches(T,[],M,M).
divisionMatches(T,[X|XS],M,ACC) :- 
	divisionMatches(T,XS,M,[(T,X),(X,T)|ACC]).



%% intraInterMatches (+T, +Ts: list, ?Ms: list) 
%
% Genera todos los posibles partidos inter e intra-divisionales
% para un equipo T, y los rivales en Ts en la lista Ms.
%
% @param T  Equipo.
% @param Ts Lista de rivales del equipo T.
% @param Ms  Lista de partidos generados.

intraInterMatches(Team,Teams,Matches) :- 
	selectn(2,[R1,R2],Teams,A),
	selectn(2,[R3,R4],A,Y),
	Matches = [(Team,R1),(Team,R2),(R3,Team),(R4,Team)].



% samePosMatches (+T, +Ts: list, ?Ms: list) 
%
% Genera todos los posibles partidos divisionales para un equipo T,
% y los rivales en Ts, todos de la misma posición pero que no forman
% parte del emparejamiento inter e intra-divisional.
%
% @param T  Equipo.
% @param Ts Lista de rivales del equipo T.
% @param Ms Lista de partidos generados.

samePosMatches(Team,Teams,Matches) :- 
	select(R1,Teams,R),
	select(R2,R,Y),
	Matches = [(Team,R1),(R2,Team)].



%% findRivInter (+C, ?X, ?Y) 
%
% Busca la unificación de elementos para la conferencia C y los
% emparejamientos inter-divisionales inter(X,Y).
%
% @param C  Conferencia.
% @param X  División de la conferencia C.
% @param Y  División que unifica.

findRivInter(afc,X,Y) :- inter(X,Y).
findRivInter(nfc,X,Y) :- inter(Y,X).


%% printDates (+N: int) 
%
% Imprime todas las semanas de un calendario, apartir de la semana N.
%
% @param N Semana.

printDates(18) :- !.
printDates(N) :- 
	write('Week '),write(N),nl,
	write('---------'),nl,
	findall((V,L),date(N,V,L),Ms),
	printTeams(Ms), nl,
	printBye(N),nl,
	M is N + 1,
	printDates(M).


%% printBye (+N: int) 
%
% Imprime los byes en caso de que la seamana N este entre 
% la 1 y la 8, ambas inclusive.
%
% @param N  Semana del calendario.

printBye(N) :- 
	N < 9, bye(N,Bye), 
	write('Bye: '), 
	write(Bye), nl, nl.
printBye(N).


%% printTeams (+L: list) 
%
% Imprime todos los enfrentamientos de equipos de la lista
% L.
%
% @param L  Lista de enfrentamientos.

printTeams([]).
printTeams([(V,L)|Xs]) :- 
	write(V), 
	write(' at '), 
	write(L),nl,
	printTeams(Xs).



%% addDate(+N: int, +R, list) 
%
% Añade todos los posibles partidos a cada semana de un equipo,
% mediante el predicado dinámico date/3.
%
% @param N Semana.	
% @param R Lista de enfrentamientos.

addDate(0,_).
addDate(N,R) :- 
	% Para las últimas semanas, juegos divisionales.
	between(14,17,N),
	select((E1,E2),R,Rest),

	% Verifica la existencia de los equipos en la fecha.
	\+date(N,E1,_), \+date(N,_,E1), \+date(N,E2,_), \+date(N,_,E2),
	verifyDivMatch(E1,E2),

	% Genera el predicado dinámico.
	asserta(date(N,E1,E2)),
	M is N - 1,
	addDate(M,Rest).

addDate(N,R) :- 
	select((E1,E2),R,Rest),

	% Verifica la existencia de los equipos en la fecha.
	\+date(N,E1,_), \+date(N,_,E1), \+date(N,E2,_), \+date(N,_,E2),

	% Verifica la lista de byes.
	verifyByes(N,E1,E2),

	% Genera el predicado dinámico.
	asserta(date(N,E1,E2)),
	M is N - 1,
	addDate(M,Rest).
addDate(N,R) :- M is N - 1, addDate(M,R).



%% verifyByes (+N: int, +E1, +E2) 
%
% Verifica si los equipos E1 y E2 están en la lista de byes.
%
% @param N  Semana del calendario.
% @param E1  Equipo visitante.
% @param E2  Equipo local.

verifyByes(N,E1,E2) :- 
	N < 9, bye(N,Bye), \+member(E1,Bye), \+member(E2,Bye).
verifyByes(N,E1,E2) :- N > 8.


%% verifyDivMatch (+N: int) 
%
% Verifica si quedan partidos divisionales por jugarse
% en las últimas semanas para los equipos E1 y E2.
%
% @param E1  Equipo visitante.
% @param E2  Equipo local.

verifyDivMatch(E1,E2):- divMatch(E1,0), divMatch(E2,0).
verifyDivMatch(E1,E2):- 
	standings(C,D,_,E1), standings(C,D,_,E2),
	divMatch(E1,N1), M1 is N1 - 1,
	divMatch(E2,N2), M2 is N2 - 1,
	asserta(divMatch(E1,M1)),
	asserta(divMatch(E2,M2)).

