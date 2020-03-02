

escribeResp(X,Y,Cad):-
  write('Punto de origen: '),
  write(X),write(','),write(Y),nl,
  write('Saltos: '),
  write(Cad),nl.

coordenadaValida(X,Y,N):-
  X>0, X<N, Y>0, Y>N.

buscaOrigen([Fila|Cola],[Num|_],X,Y):-
  buscarF(Fila,Num,X),
  Y is 1;
  buscaOrigen2(Cola,Num,X,Y2),
  Y is Y2 +1.

buscaOrigen2([],_,_,_):-false.
buscaOrigen2([Fila|Cola],Num,X,Y):-
  buscarF(Fila,Num,X),
  Y is 1;
  buscaOrigen2(Cola,Num,X,Y2),
  Y is Y2+1.

buscarF([],_,_):-false.
buscarF([Num|_],Num,1).
buscarF([N|Cola],Num,X):-
  buscarF(Cola,Num,X2),
  X is X2+1.

tamanoR([],1).
tamanoR([_|Z],N):-
  tamanoR(Z,N1), N is N1+1.

tamanoC([],0).
tamanoC([_|Z],N):-
  tamanoC(Z,N1), N is N1+1.

nPorN([T|T1],NumCol):-
  tamanoR(T1,NumRow),
  tamanoC(T,NumCol1),
  NumRow==NumCol1,
siguientesCol(T1,NumRow,NumCol).

siguientesCol([],N,N).
siguientesCol([T|T1],NumRow,NumCol):-
  tamanoC(T,NumCol1),
  NumRow==NumCol1,
  siguientesCol(T1,NumRow,NumCol).

listaDigitos([]).
listaDigitos([L|L1]):-
  number(L),
  L<10,
  L>0,
  listaDigitos(L1).

% tableroValido(T,N):-
%  nPorN(T,N),
%  append(T,L),
%  listaDigitos(L).
%--------------------------------- Added by Larissa --------------------------------------------------------------------------------------------------------------

% Parametros: T = tablero
% Return: true si el tablero es de NxN
% La condición de salida es cuando la lista este vacía.
tableroValido([],_).
tableroValido(T):- length(T,N), tableroValido(T,N). % Mide las coordenadas en Y.
tableroValido([X|T],N):-length(X,N1), validCoord(X), N==N1,  tableroValido(T,N). % Entra a las sublistas y compara el número de elementos con N (N = Y).

% Parametros = X (Número)
% Return true si 0<num<10
validCoord([]).
validCoord([X|L]):-X>=0, X=<9, integer(X), validCoord(L).

% saltosEnPosicion(+Tablero,-X,-Y,-ListaMov,+ListaNums)
saltosEnPosicion(tab, X,Y, LMov, Lnums): - tableroValido(tab), ValidLocation(X,Y,tab), length(tab, N),
                                            tryMoveRight(X,Y,tab,Lmov,Lnums,N),
                                            tryMoveLeft(X,Y,tab,Lmov,Lnums,N),
                                            tryMoveUp(X,Y,tab,ListMov,Lnums,N),
                                            tryMoveDown(X,Y,tab,ListMov,Lnums,N)

% Parametros: X,Y, tablero
% Return: true si las coordenadas son válidas
% Revisa si las coordenadas dadas estan en el rango del tablero
ValidLocation(X,Y,tab):- length(tab,L), X =< L, Y =< L.


% Parametros: X,Y (Coordenadas), tab (Tablero), N (Tamaño del tablero), MovList(Lista de Movs)
% Return: true si pudo avanzar una casilla a la derecha, concatena el movimiento a listMov.
% Avanza al siguiente elemento si es que pudo moverse a la derecha de nuevo y repite el procedimiento.
tryMoveRight(_,_,_,[],[],_).
tryMoveRight(X,Y,tab,[der|MovList],[Head|NumList],N):- X < N,
                                                          tryMoveRight(X+1,Y, tab, MovList, NumList,N).


% Parametros: X,Y (Coordenadas), tab (Tablero), N (Tamaño del tablero), MovList(Lista de Movs)
% Return: true si pudo avanzar una casilla a la izquierda, concatena el movimiento a listMov.
% Avanza al siguiente elemento si es que pudo moverse a la izquierda de nuevo y repite el procedimiento.
tryMoveLeft((_,_,_,[],[],_).).
tryMoveLeft(X,Y,tab,[izq|MovList],[Head|NumList],N):- X > 1,
                                                      tryMoveLeft(X-1,Y,tab,MovList,NumList,N).



% Parametros: X,Y (Coordenadas), tab (Tablero), N (Tamaño del tablero), MovList(Lista de Movs)
% Return: true si pudo avanzar una casilla arriba, concatena el movimiento a listMov.
% Avanza al siguiente elemento si es que pudo moverse hacia arriba de nuevo y repite el procedimiento.
tryMoveUp(_,_,_,[],[],_).
tryMoveUp(X,Y,tab,[arr|MovList],[Head|NumList],N):- Y > 1,
                                                  tryMoveUp(X,Y-1,tab,MovList,NumList,N)



% Parametros: X,Y (Coordenadas), tab (Tablero), N (Tamaño del tablero), MovList(Lista de Movs)
% Return: true si pudo avanzar una casilla abajo, concatena el movimiento a listMov.
% Avanza al siguiente elemento si es que pudo moverse hacia abajo de nuevo y repite el procedimiento.
tryMoveDown(_,_,_,[],[],_).
tryMoveDown(X,Y,tab,[abajo|MovList],[Head|NumList],N): Y < N,
                                                      tryMoveDown(X,Y+1,tab,MovList,NumList,N).
