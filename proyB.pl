tablero([[1,4,5,8],[7,4,1,9],[2,5,5,6],[9,8,3,8]]).

main(T,ListaNums,Sol):-
  tableroValido(T,N),!,
  buscaOrigen(T,ListaNums,X,Y),
  buscarCadena(T,ListaNums,X,Y,Cad),
  escribeResp(X,Y,Cad),
  fail.
main(T,ListaNums,Sol).

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

tableroValido(T,N):-
  nPorN(T,N),
  append(T,L),
  listaDigitos(L).
%--------------------------------- Added by Larissa --------------------------------------------------------------------------------------------------------------

% Parametros: tablero
% Return: true si el tablero es de NxN
% La condición de salida es cuando la lista este vacía.
tableroValido([],_).
tableroValido(L):- length(L,N), tableroValido(L,N). % Entra a lista y se mide la longitud.
tableroValido([X|L],N):- length(X,N1), coordenadaValida(X,N), N==N1,  tableroValido(L,N). % Revisa renglon a renglon el tamaño, revisa 0<x<10 con coordenadaValida.

% saltosEnPosicion(+Tablero,-X,-Y,-ListaMov,+ListaNums)
saltosEnPosicion(tab, X,Y, LMov, LNums): - tableroValido(tab), ValidLocation(X,Y),
                                            tryMoveRight(X,Y,tab, ListMov, N),
                                            tryMoveRight(X,Y,tab, L)
                                            tryMoveLeft(X,Y,ListMov, N),
                                            tryMoveUp(X,Y,ListMov, N);
                                            tryMoveDown(X,Y,ListMov, N)

% Parametros: X,Y, tablero
% Return: true si las coordenadas son válidas
% Revisa si las coordenadas dadas estan en el rango del tablero
ValidLocation(X,Y,[]):- length(tab,L), X =< L, Y =< L.


% Parametros: X,Y (Coordenadas), tab (Tablero), N (Tamaño del tablero), listMov(Lista de Movs).
% Return: true si pudo avanzar una casilla a la derecha, concatena el movimiento a listMov.
% Avanza al siguiente elemento si es que pudo moverse a la derecha de nuevo y repite el procedimiento.
tryMoveRight(X, Y, tab, N, listMov):-  X < N,
                          append(ListMov, "Este"),
                          tryMoveRight(X+1,Y,(Head|Tail), ListMov,N).

% tryMoveLeft(X,Y,ListMov, N):
% tryMoveUp(X,Y,ListMov, N):
% tryMoveDown(X,Y,ListMov, N):
