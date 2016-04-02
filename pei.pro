% Universidad Simón Bolívar
% Departamento de Computación y Tecnología de la Información
% CI3661 - Laboratorio de Lenguajes de Programación I
% Trimestre Enero - Marzo 2016.
% 
% Tarea Prolog.
%
% Pares e Impares.
%  
% Autores:          Gabriel Iglesias 11-10476.
%                   Oscar Guillen    11-11264.
% Grupo:     		G16
% Última edición: 	31 de marzo de 2016.

%% pei 
%
%  Halla el valor que deben tener los digitos de la multiplicación
% 
%   IPP *
%    PP
%  ----
%  PIPP +
%  PIP
%  ----
%  IIPP
%
%  de modo que los dígitos marcados con p sean pares y los dígitos marcados 
%  con i sean impares.

pei :-

	% Declaramos los arreglos de variables libres para pares e impares.
	Ps = [P0, P1, P2, P3],
	Is = [I0],

	% Unificamos variables.    
	Impares = [1, 3, 5, 7, 9],
	assign(Is, Impares),

	Pares = [0, 2, 4, 6, 8],
	assign(Ps, Pares),

	% Se establecen las condiciones para simplificar la búsqueda. 
	% En caso de no cumplirse con las condiciones, se usa backtracking y se 
	% realiza una nueva asignación de los valores.
	P2 > 0,
	P3 > 0,  

	% Evaluación de la multiplicación.
	IPP  is 100*I0 + 10*P1 + P0,
	PP   is 10*P3 + P2,
    
	PIPP is IPP * P2,
	PIPP >  999,
    
	PIP  is IPP * P3,
	PIP  <  1000,

	IIPP is PIPP + PIP*10,
	IIPP <  10000,

	% Se determinan los dígitos del resultado de la multiplicación IPP * P2.
	P6 is  PIPP//1000,
	P6 mod 2 =:= 0,
	I1 is (PIPP - P6*1000)//100,
	I1 mod 2 =\= 0,
	P5 is (PIPP - (P6*1000 + I1*100))//10,
	P5 mod 2 =:= 0,
	P4 is PIPP - (P6*1000 + I1*100 + P5*10),
	P4 mod 2 =:= 0,
	
	% Se determinan los dígitos del resultado de la multiplicación IPP * P3.
	P8 is PIP//100,
	P8 mod 2 =:= 0,
	P8 > 0, 
	I2 is (PIP - P8*100)//10,
	I2 mod 2 =\= 0,
	P7 is PIP - (P8*100 + I2*10),
	P7 mod 2 =:= 0,
	
	% % Se determinan los dígitos del resultado final de la multiplicación.
	I4 is IIPP//1000,
	I4 mod 2 =\= 0,
	I3 is (IIPP - I4*1000)//100,
	I3 mod 2 =\= 0,
	P9 is (IIPP - (I4*1000 + I3*100))//10,
	P9 mod 2 =:= 0,
    
	% Imprime la solución.
	printSolution(IPP, PP, PIPP, PIP, IIPP).

%% printSolution (+IPP: int, +PP: int, +PIPP: int, +PIP: int, +IIPP: int) 
%
% Imprime la multiplicación con los valores de los dígitos hallados por el 
% predicado pei.
%
% @param IPP  primer factor de la multiplicación
% @param PP   segundo factor de la multiplicación
% @param PIPP primer sumando resultado de la multiplicación
% @param PIP  segundo sumando resultado de la multiplicación 
% @param IIPP resultado final de la multiplicación

printSolution(IPP, PP, PIPP, PIP, IIPP) :-
	nl,
	write('   '),    
	write(IPP),
	write(' *'),
	nl,
	write('    '),
	write(PP),
	nl,
	write('  ____'),
	nl,
	write('  '),
	write(PIPP),
	write(' +'),
	nl,
	write('  '),
	write(PIP),
	nl,
	write('  ____'),
	nl,
	write('  '),
	write(IIPP).


%% assign (?L1: list +L2: list)
%
% Unifica las variables de la lista L1 con los valores de la lista L2.
%
% @param [H | T] lista de variables a las que se les va a unificar un valor 
%                perteneciente a la lista List. 
% @param List    lista de valores que pueden unificarse con las variables de 
%                la lista [H | T]

assign([], List).
assign([H | T], List) :- member(H, List), assign(T, List).
