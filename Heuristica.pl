% Idea intuitiva: implementar una regla que calcula la puntuación heurística de un estado del tablero para el algoritmo minimax:
% - Directamente, máxima heurística posible si se asegura la victoria, y mínima si se asegura la victoria del contrincante
% - Una función ponderada lineal que asigna pesos a ciertas características del juego:
%   · Impedir ganar al contrincante
%   · Tener una raya de tres
%   · Impedir que el contrincante forme una raya de tres
%   · Tener una raya de dos
%   · Impedir que el contrincante forme una raya de dos

% Vamos a usar reglas definidas en este fichero
:- consult("Minimax.pl").

% Para depuración más fácil del algoritmo minimax (reduce los tamaños de la resolvente a trazar)
%heuristica(_, V) :- random(0, 20000, V).

% Predicados que obtiene la puntuación heurística del estado actual del tablero
% Si yo he ganado, la heurística será la máxima
heuristica(_, Valor) :-
	jugadorGano(true, true),
	heuristicaVictoria(Valor).
% Si el contrincante ha ganado, la heurística será la mínima
heuristica(_, Valor) :-
	jugadorGano(false, true),
	heuristicaDerrota(Valor).
% En otro caso (nadie ha ganado), la heurística se calculará en base a una función ponderada lineal
heuristica(_, Valor) :-
	jugadorGano(true, false),
	jugadorGano(false, false),
	heuristicaPonderadaLineal(Valor).

% Calcula una puntuación heurística a partir de características del tablero que se consideran positivas (y negativas)
heuristicaPonderadaLineal(Valor) :-
	caracteristicaImpedirRaya(true, CaracteristicaImpedirRaya3, 3),
	caracteristicaRaya(true, CaracteristicaRaya3Mia, 3),
	caracteristicaRaya(true, CaracteristicaRaya2Mia, 2),
	caracteristicaFichasEnCentro(true, CaracteristicaFichasCentroYo),
	Valor is 30 * CaracteristicaImpedirRaya3 + 30 * CaracteristicaRaya3Mia + 20 * CaracteristicaRaya2Mia + CaracteristicaFichasCentroYo.

% Computa la característica de formar una raya de N fichas
caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :-
	findall(_, rayaSimulada(_, _, Fichas, _, _, Yo), L),
	length(L, CaracteristicaRaya).

% Cláusula interfaz para computar el valor de la característica de impedir la formación de una raya al rival
caracteristicaImpedirRaya(_, 0, 0).
caracteristicaImpedirRaya(Yo, CaracteristicaImpedirRaya, Fichas) :-
	FichasProc is Fichas - 1,
	FichasProc > 0,
	negar(Yo, Otro),
	findall(raya(CX, CY, FichasProc, DX, DY, Otro), rayaSimulada(CX, CY, FichasProc, DX, DY, Otro), RayasSimuladas),
	findall(raya(CX2, CY2, FichasProc, DX2, DY2, Otro), raya(CX2, CY2, FichasProc, DX2, DY2, Otro), Rayas),
	append(Rayas, RayasSimuladas, TodasRayas),
	caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, FichasProc, TodasRayas, 0).
% Sin más rayas que procesar, el valor de la heurística se queda como está (caso base)
caracteristicaImpedirRaya_impl(_, CaracteristicaImpedirRaya, _, [], CaracteristicaImpedirRaya).
% Mientras queden rayas que procesar, incrementar la heurística si tenemos una ficha en una posición que bloquee esa raya
caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, Fichas, [raya(CX, CY, Fichas, DX, DY, _)|Cdr], CaracteristicaActual) :-
	casillasOcupadas(CX, CY, Fichas, DX, DY, Casillas),
	ultimoElemento(Casillas, casilla(X, Y)),
	XBloq is X + DX,
	YBloq is Y + DY,
	incrementarSiImpide(Yo, XBloq, YBloq, CaracteristicaActual, NuevaCaracteristica),
	caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, Fichas, Cdr, NuevaCaracteristica).

% Predicados que incrementan el valor de CaracteristicaActual dependiendo de si se cumple la condición
% de bloqueo o no
incrementarSiImpide(Yo, XBloq, YBloq, CaracteristicaActual, CaracteristicaActual) :-
	soyYoAIdentificadorJugador(Yo, Id),
	not(tablero(XBloq, YBloq, Id)).
incrementarSiImpide(Yo, XBloq, YBloq, CaracteristicaActual, NuevaCaracteristica) :-
	soyYoAIdentificadorJugador(Yo, Id),
	tablero(XBloq, YBloq, Id),
	NuevaCaracteristica is CaracteristicaActual + 1.

% Computa la característica de tener fichas en el centro
caracteristicaFichasEnCentro(Yo, Valor) :-
	soyYoAIdentificadorJugador(Yo, Id),
	findall(_, tablero(3, _, Id), L1),
	length(L1, CaracteristicaFichasCentro1),
	findall(_, tablero(4, _, Id), L2),
	length(L2, CaracteristicaFichasCentro2),
	Valor is CaracteristicaFichasCentro1 + CaracteristicaFichasCentro2.

% Comprueba si un jugador ha ganado; es decir, ha formado una raya de 4
jugadorGano(Yo, false) :- not(rayaSimulada(_, _, 4, _, _, Yo)).
jugadorGano(Yo, true) :- rayaSimulada(_, _, 4, _, _, Yo).