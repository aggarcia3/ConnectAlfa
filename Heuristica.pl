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
	caracteristicaRaya(true, CaracteristicaRaya3Mia, 3),
	caracteristicaRaya(false, CaracteristicaRaya3Rival, 3),
	caracteristicaRaya(true, CaracteristicaRaya2Mia, 2),
	caracteristicaRaya(false, CaracteristicaRaya2Rival, 2),
	Valor is 6 * CaracteristicaRaya3Mia - 6 * CaracteristicaRaya3Rival + 4 * CaracteristicaRaya2Mia - 4 * CaracteristicaRaya2Rival.

caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :-
	findall(_, rayaSimulada(_, _, Fichas, _, _, Yo), L),
	length(L, CaracteristicaRaya).

% Comprueba si un jugador ha ganado; es decir, ha formado una raya de 4
jugadorGano(Yo, false) :- not(rayaSimulada(_, _, 4, _, _, Yo)).
jugadorGano(Yo, true) :- rayaSimulada(_, _, 4, _, _, Yo).