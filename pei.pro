%    I0 P1 P0 *
%       P3 P2
% ____________
% P6 I1 P5 P4 +
% P8 I2 P7
% ____________
% I4 I3 P9 P4

pei :-

	% Declaramos variables libres
    Ps = [P0, P1, P2, P3],
    Is = [I0],

	% Unificamos variables.    
	Impares = [1, 3, 5, 7, 9],
    assign(Is, Impares),

    Pares = [0, 2, 4, 6, 8],
    assign(Ps, Pares),

	% Verifica las condiciones para simplificar la búsqueda. 
	% En caso de no cumplirse con las condiciones, se usa backtracking y se 
	% realiza una nueva asignación de los valores.
	
	P2 > 0,
    P3 > 0,  

	% Evaluación
    IPP  is 100*I0 + 10*P1 + P0,
    PP   is 10*P3 + P2,
    
	PIPP is IPP * P2,
	PIPP >  999,
    
	PIP  is IPP * P3,
	PIP  <  1000,

	IIPP is PIPP + PIP*10,
	IIPP <  10000,

	% Multiplicación IPP * P2
	P6 is  PIPP//1000,
	P6 mod 2 =:= 0,
	I1 is (PIPP - P6*1000)//100,
	I1 mod 2 =\= 0,
	P5 is (PIPP - (P6*1000 + I1*100))//10,
	P5 mod 2 =:= 0,
	P4 is PIPP - (P6*1000 + I1*100 + P5*10),
	P4 mod 2 =:= 0,

	% Multiplicación IPP * P3
	P8 is PIP//100,
	P8 mod 2 =:= 0,
	P8 > 0, 
	I2 is (PIP - P8*100)//10,
	I2 mod 2 =\= 0,
	P7 is PIP - (P8*100 + I2*10),
	P7 mod 2 =:= 0,

	% Suma	
	I4 is IIPP//1000,
	I4 mod 2 =\= 0,
	I3 is (IIPP - I4*1000)//100,
	I3 mod 2 =\= 0,
	P9 is (IIPP - (I4*1000 + I3*100))//10,
	P9 mod 2 =:= 0,
    
	% Imprime la solución.
	printSolution(IPP, PP, PIPP, PIP, IIPP).

printSolution(IPP, PP, PIPP, PIP, IIPP) :-
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

% Asigna los números de List a la lista compuesta [D|Ds] en algún orden.
assign([], List).
assign([D|Ds], List) :-
    member(D, List),    
    assign(Ds, List).
