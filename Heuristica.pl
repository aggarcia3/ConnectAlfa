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
heuristica(_, V) :- random(0, 20000, V).

% Predicados que obtiene la puntuación heurística del estado actual del tablero
% Si yo he ganado, la heurística será la máxima
%heuristica(Jugada, Valor) :-
%	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado),
%	ValorVerdadGanado,
%	heuristicaVictoria(Valor).
% Si el contrincante ha ganado, la heurística será la mínima
%heuristica(Jugada, Valor) :-
%	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido),
%	ValorVerdadPerdido,
%	heuristicaDerrota(Valor).
% En otro caso (nadie ha ganado), la heurística se calculará en base a una función ponderada lineal
%heuristica(Jugada, Valor) :-
%	jugadorGano_cacheado(Jugada, true, ValorVerdadGanado),
%	jugadorGano_cacheado(Jugada, false, ValorVerdadPerdido),
%	not(ValorVerdadGanado), not(ValorVerdadPerdido),
%	heuristicaPonderadaLineal(Valor).

% Calcula una puntuación heurística a partir de características del tablero que se consideran positivas (y negativas)
heuristicaPonderadaLineal(Valor) :-
	caracteristicaImpedirRaya(CaracteristicaImpedirVictoria, 4),
	caracteristicaRaya(true, CaracteristicaRaya3Mia, 3),
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya3, 3),
	caracteristicaRaya(false, CaracteristicaRaya3Rival, 3),
	caracteristicaImpedirRaya(CaracteristicaImpedirRaya2, 2),
	caracteristicaRaya(true, CaracteristicaRaya2Mia, 2),
	caracteristicaRaya(false, CaracteristicaRaya2Rival, 2),
	caracteristicaFichasEnCentro(true, CaracteristicaFichasCentro),
	Valor is 2000 * CaracteristicaImpedirVictoria + 6 * CaracteristicaRaya3Mia + 6 * CaracteristicaImpedirRaya3 - 7 * CaracteristicaRaya3Rival + 4 * CaracteristicaRaya2Mia + 4 * CaracteristicaImpedirRaya2 - 5 * CaracteristicaRaya2Rival + CaracteristicaFichasCentro.

% Cláusula interfaz que computa la característica de impedir la formación de una raya de N fichas del rival
caracteristicaImpedirRaya(CaracteristicaImpedirRaya, Fichas) :- caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, 0, 0, 0).
% Si en la posición actual hemos impedido una raya de N fichas, considerarlo para la característica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	impidoRaya(true, X, Y, Fichas),
	XSig is X + 1,
	NuevoAcumulador is Acumulador + 1, % TODO: + n, siendo N el número de maneras posibles de impedir raya
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, XSig, Y, NuevoAcumulador).
% Si en la posición actual no hemos impedido una raya de N fichas, pero podemos seguir incrementando X, hacer eso
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	not(impidoRaya(true, X, Y, Fichas)),
	XSig is X + 1,
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, XSig, Y, Acumulador).
% Si hemos agotado las posiciones X de la fila actual, ir con la siguiente fila
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X == Ancho, Y < Alto,
	YSig is Y + 1,
	caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, Fichas, 0, YSig, Acumulador).
% Si hemos llegado al final de las filas del tablero, es que hemos acabado, y el acumulador contiene
% el valor final de la característica
caracteristicaImpedirRaya_impl(CaracteristicaImpedirRaya, _, _, Y, CaracteristicaImpedirRaya) :-
	altoTablero(Alto),
	Y >= Alto.

% Cláusula interfaz que computa la característica de formar una raya de N fichas 
caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :- caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, 0, 0, 0).
% Si en la posición actual hemos formado una raya de N fichas, considerarlo para la característica
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	raya(Yo, X, Y, Fichas),
	XSig is X + 1,
	NuevoAcumulador is Acumulador + 1,
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, XSig, Y, NuevoAcumulador).
% Si en la posición actual no hemos formado una raya de N fichas, pero podemos seguir incrementando X, hacer eso
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	not(raya(Yo, X, Y, Fichas)),
	XSig is X + 1,
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, XSig, Y, Acumulador).
% Si hemos agotado las posiciones X de la fila actual, ir con la siguiente fila
caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, X, Y, Acumulador) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X == Ancho, Y < Alto,
	YSig is Y + 1,
	caracteristicaRaya_impl(Yo, CaracteristicaRaya, Fichas, 0, YSig, Acumulador).
% Si hemos llegado al final de las filas del tablero, es que hemos acabado, y el acumulador contiene
% el valor final de la característica
caracteristicaRaya_impl(_, CaracteristicaRaya, _, _, Y, CaracteristicaRaya) :-
	altoTablero(Alto),
	Y >= Alto.

% Cláusula interfaz que computa la característica de tener fichas en las 4 posiciones centrales del tablero
caracteristicaFichasEnCentro(Yo, CaracteristicaFichasCentro) :-
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro1, 3, 3),
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro2, 4, 3),
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro3, 3, 4),
	caracteristicaFichasEnCentro_impl(Yo, CaracteristicaFichasCentro4, 4, 4),
	CaracteristicaFichasCentro is CaracteristicaFichasCentro1 + CaracteristicaFichasCentro2 + CaracteristicaFichasCentro3 + CaracteristicaFichasCentro4.
% Si en la posición dada hay una ficha nuestra, la característica es favorable
caracteristicaFichasEnCentro_impl(Yo, 1, X, Y) :-
	soyYoAIdentificadorJugador(Yo, Id),
	tablero(X, Y, Id).
% Si en la posición dada no hay una ficha, o no es nuestra, la característica es desfavorable
caracteristicaFichasEnCentro_impl(Yo, 0, X, Y) :-
	soyYoAIdentificadorJugador(Yo, MiId),
	tablero(X, Y, Id), Id \= MiId.

% Si ya hemos computado el resultado de jugadorGano para la jugada actual, reusarlo. En caso contrario, computarlo una vez
% por jugada
:- dynamic jugadorGano_/3.
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :-
	not(jugadorGano_(Jugada, Yo, ValorVerdad)),
	jugadorGano(Yo, ValorVerdad), % El parámetro Jugada tan solo nos interesa para distinguir entre estados del tablero y garantizar coherencia de caché, no para computar si el jugador ha ganado
	assertz(jugadorGano_(Jugada, Yo, ValorVerdad)).
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :- jugadorGano_(Jugada, Yo, ValorVerdad).
% Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrar_jugadorGano_cacheado :- retractall(jugadorGano_(_, _, _)).

% Cláusula interfaz para comprobar si alguien ha ganado la partida o no. El segundo argumento existe para que se pueda guardar
% en caché el valor de salida de tal argumento, en vez de si se ha encontrado una solución o no.
% Diferencia principal entre esta regla y caracteristicaRaya: detiene la evaluación de rayas al encontrar la primera de 4,
% y no sigue hasta el final del tablero, por lo que es algo más eficiente
jugadorGano(_, false).% :- jugadorGano_impl(0, 0, Yo, ValorVerdad).

% Ha ganado si partiendo de esta casilla hay 4 en raya en cualquiera de las direcciones posibles
jugadorGano_impl(X, Y, Yo, true) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	raya(Yo, X, Y, 4).
% Si no hay un 4 en raya en cualquiera de las direcciones posibles, y podemos ver si lo hay en la siguiente coordenada X,
% hacer tal comprobación
jugadorGano_impl(X, Y, Yo, ValorVerdad) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X < Ancho, Y < Alto,
	not(raya(Yo, X, Y, 4)), % Para evitar backtracking por aquí. Quizás sea buena idea quitarlo si eso no es problema
	XSig is X + 1,
	jugadorGano_impl(XSig, Y, Yo, ValorVerdad).
% Si no hay siguiente coordenada X para esta Y, probar con la siguiente Y, empezando otra vez en 0 en X
jugadorGano_impl(X, Y, Yo, ValorVerdad) :-
	anchoTablero(Ancho), altoTablero(Alto),
	X == Ancho, Y < Alto,
	YSig is Y + 1,
	jugadorGano_impl(0, YSig, Yo, ValorVerdad).
% Si acabamos de recorrer el tablero y no hemos determinado que alguien haya ganado, entonces
% es que no ha ganado
jugadorGano_impl(_, Y, _, false) :-
	altoTablero(Alto),
	Y >= Alto.

% Regla que es cierta si y solo si un jugador ha hecho una raya de N fichas, considerando la posición (X, Y) como la ficha
% central de la hipotética raya
raya(Yo, X, Y, Fichas) :-
	fichaEn(Yo, X, Y),
	FichasRestantes is Fichas - 1,
	(rayaEnDireccion(Yo, X, Y, FichasRestantes, 1, 0);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, 0, 1);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, 1, 1);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, -1, 1);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, 1, -1);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, -1, 0);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, 0, -1);
	rayaEnDireccion(Yo, X, Y, FichasRestantes, -1, -1)).
% Cláusula interfaz para comprobar si, en el vector de dirección dado, hay N fichas más a partir de (X, Y) que pueden formar cuatro en raya
rayaEnDireccion(Yo, X, Y, Fichas, DX, DY) :- rayaEnDireccion_impl(Yo, X, Y, Fichas, 1, DX, DY).
% Una raya de una ficha siempre se cumple en nuestro caso
rayaEnDireccion_impl(_, _, _, 0, _, _, _).
% Ver si la raya en esta dirección se mantiene hasta agotar el número de fichas deseado
rayaEnDireccion_impl(Yo, X, Y, Fichas, FichasContadas, DX, DY) :-
	Fichas > 0,
	X1 is X + DX * FichasContadas,
	Y1 is Y + DY * FichasContadas,
	fichaEn(Yo, X1, Y1),
	NuevasFichas is Fichas - 1,
	NuevasFichasContadas is FichasContadas + 1,
	rayaEnDireccion_impl(Yo, X, Y, NuevasFichas, NuevasFichasContadas, DX, DY).

% Regla que es cierta si y solo si un jugador impide al otro hacer una raya de N fichas, considerando la posición (X, Y)
% como la ficha central de la raya del otro jugador (que debe de existir)
impidoRaya(Yo, X, Y, Fichas) :-
	negar(Yo, Otro),
	fichaEn(Otro, X, Y),
	FichasRestantes is Fichas - 1,
	(impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, 1, 0);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, 0, 1);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, 1, 1);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, -1, 1);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, 1, -1);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, -1, 0);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, 0, -1);
	impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, -1, -1)).
% Cláusula interfaz para comprobar si, en el vector de dirección dado, impido una raya de N fichas a partir de (X, Y), colocando una
% a distancia N + 1
impidoRayaEnDireccion(Yo, X, Y, FichasRestantes, DX, DY) :-
	NuevasFichasRestantes is FichasRestantes - 1,
	negar(Yo, Otro),
	rayaEnDireccion(Otro, X, Y, NuevasFichasRestantes, DX, DY), % Solo se puede impedir una raya en la misma dirección
	X1 is X + DX * FichasRestantes,
	Y1 is Y + DY * FichasRestantes,
	fichaEn(Yo, X1, Y1).

% Regla que es cierta si y solo si la posición (X, Y) tiene una ficha mía o del otro jugador
fichaEn(Mia, X, Y) :-
	soyYoAIdentificadorJugador(Mia, Id),
	tablero(X, Y, Id).