% Idea intuitiva: guardar las últimas jugadas procesadas por minimax, para que si se vuelven a consultar
% baste con consultar una tabla en lugar de tener que calcular minimax de nuevo

:- dynamic hashZobristActual/1.
% Calcula el primer valor del hash de Zobrist para el estado inicial del tablero.
% Implementación basada en https://en.wikipedia.org/wiki/Zobrist_hashing
inicializarZobrist :-
	inicializarTablasZobrist,
	hashZobrist(Hash),
	asserta(hashZobristActual(Hash)).

:- dynamic tablaZobrist/4.
% Crea las estructuras de datos necesarias para utilizar la función de
% dispersión Zobrist
inicializarTablasZobrist :-
	writeln("Inicializando tablas de función de dispersión Zobrist..."),
	inicializarTablasZobrist_impl(0, 0, 0).
inicializarTablasZobrist_impl(X, Y, Id) :-
	X < 8, Y < 8, Id < 3,
	% 9007199254740992 = 2^53. Por tanto, Zobrist usará claves de 53 bits.
	% Este número es el mayor double para el cual la distancia al anterior
	% representable es menor o igual que 1. Véase NumberTermImpl.java en Jason
	random(0.0, 1.0, R),
	Num is floor(9007199254740992 * R),
	assertz(tablaZobrist(X, Y, Id, Num)),
	IdSig is Id + 1,
	inicializarTablasZobrist_impl(X, Y, IdSig).
% Si no hay más posibles jugadas en la casilla actual, moverse a la siguiente
inicializarTablasZobrist_impl(X, Y, Id) :-
	X < 8, Y < 8, Id = 3,
	XSig is X + 1,
	inicializarTablasZobrist_impl(XSig, Y, 0).
% Si no hay más posibles jugadas en la fila actual, moverse a la siguiente
inicializarTablasZobrist_impl(X, Y, _) :-
	X = 8, Y < 7,
	YSig is Y + 1,
	inicializarTablasZobrist_impl(0, YSig, 0).
% Si hemos terminado de recorrer las posibles entradas, terminar la recursividad
inicializarTablasZobrist_impl(X, Y, _) :-
	writeln("Tablas de la función de dispersión Zobrist inicializadas"),
	X = 8, Y = 7.

% Calcula el valor de dispersión (hash) del estado actual del tablero
hashZobrist(Hash) :-
	writeln("Calculando valor de dispersión Zobrist inicial..."),
	hashZobrist_impl(0, 0, Hash, 0).
hashZobrist_impl(X, Y, Hash, HashActual) :-
	X < 8, Y < 8,
	tablero(X, Y, Id),
	tablaZobrist(X, Y, Id, Num),
	NuevoHash is HashActual xor Num, % Implementar con internal action en Jason
	XSig is X + 1,
	hashZobrist_impl(XSig, Y, Hash, NuevoHash).
% Si hemos terminado de procesar la fila actual, ir con la siguiente
hashZobrist_impl(X, Y, Hash, HashActual) :-
	X = 8, Y < 7,
	YSig is Y + 1,
	hashZobrist_impl(0, YSig, Hash, HashActual).
% Si hemos terminado de procesar todo el tablero, tenemos el hash final
hashZobrist_impl(X, Y, Hash, Hash) :-
	writeln("Valor de dispersión Zobrist inicial calculado"),
	X = 8, Y = 7.

% Actualiza el hash Zobrist calculado con una colocación de una ficha en el tablero
colocarFichaZobrist(X, Y, AnteriorId, NuevoId) :-
	hashZobristActual(Hash),
	tablaZobrist(X, Y, AnteriorId, AnteriorNum),
	HashProcAux is Hash xor AnteriorNum,
	tablaZobrist(X, Y, NuevoId, NuevoNum),
	HashProc is HashProcAux xor NuevoNum,
	retract(hashZobristActual(_)),
	asserta(hashZobristActual(HashProc)).

:- dynamic entradasMapaZobrist/1.
% El número de entradas actuales en la tabla de transposiciones
entradasMapaZobrist(0).

:- dynamic mapaZobrist/2.
% Crea una entrada en la tabla de transposiciones para el estado actual.
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas),
	Entradas < 65536, % A 1 KiB por entrada (estimación a ojo de buen cubero pesimista), las entradas ocuparían 65 MiB
	eliminarDeMapaSiEsta,
	hashZobristActual(Hash),
	retract(entradasMapaZobrist(_)),
	EntradasInc is Entradas + 1,
	asserta(entradasMapaZobrist(EntradasInc)),
	assertz(mapaZobrist(Hash, E)).
% Si llegamos al máximo de entradas, vaciar el mapa y volver a empezar
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas),
	Entradas >= 65536,
	retract(mapaZobrist(_, _)), % Implementar como función interna en Jason
	hashZobristActual(Hash),
	assertz(mapaZobrist(Hash, E)).

% Elimina la entrada en la tabla correspondiente al estado actual, si hay
eliminarDeMapaSiEsta :-
	enMapaZobrist,
	hashZobristActual(Hash),
	entradasMapaZobrist(Entradas),
	retract(entradasMapaZobrist(_)),
	EntradasDec is Entradas - 1,
	asserta(entradasMapaZobrist(EntradasDec)),
	retract(mapaZobrist(Hash, _)).
eliminarDeMapaSiEsta :-
	not(enMapaZobrist).

% Comprueba si hay una entrada para el estado actual del tablero en la tabla de
% transposiciones
enMapaZobrist :-
	hashZobristActual(Hash),
	mapaZobrist(Hash, _).

% Obtiene la entrada asociada al estado actual del tablero en la tabla de
% transposiciones. No maneja colisiones, que se estiman muy poco probables
obtenerEntradaZobrist(E) :-
	hashZobristActual(Hash),
	mapaZobrist(Hash, E).

:- inicializarZobrist.