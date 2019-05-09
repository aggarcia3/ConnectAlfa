// Agente player.asl en proyecto ConnectAlfa.mas2j

/* Creencias y reglas iniciales */

// La profundidad máxima del árbol de juego a considerar. Afecta a la
// corrección y tiempo de ejecución del algoritmo: más profundidad aumenta
// la calidad de las jugadas, a costa de mayor tiempo de ejecución y consumo de
// memoria
profundidadArbolJuego(4).

// El valor devuelto por la heurística para señalar una victoria. Es el valor
// máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
// El valor devuelto por la heurística para señalar una derrota. Es el valor
// mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).

// Cláusulas interfaz para obtener la columna donde colocar una ficha para maximizar
// nuestra victoria o la del contrincante
mejorSiguienteColumna(X) :- minimax([[movimiento(X, _, _)|_], _]).
peorSiguienteColumna(X) :- maximin([[movimiento(X, _, _)|_], _]).

// Cláusula interfaz para obtener la jugada óptima a realizar, con su heurística asociada
minimax(JugadaYHeuristica) :-
	profundidadArbolJuego(P) &
	heuristicaDerrota(MinimoMax) & // MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo
	heuristicaVictoria(MaximoMin) & // MaximoMin = Beta
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, false) &
	// Borrar datos en caché y simulaciones temporales
	borrar_jugadorGano_cacheado &
	deshacerJugadaHecha.
// Cláusula interfaz para obtener la jugada óptima a realizar para perder en el juego, con su heurística asociada
maximin(JugadaYHeuristica) :-
	profundidadArbolJuego(P) &
	heuristicaDerrota(MinimoMax) & // MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo que una letra griega
	heuristicaVictoria(MaximoMin) & // MaximoMin = Beta
	.asserta(haciendoMaximin(true)) & // Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, true) &
	.abolish(haciendoMaximin(_)) &
	// Borrar datos en caché y simulaciones temporales
	borrar_jugadorGano_cacheado &
	deshacerJugadaHecha.
// Si la profundidad restante es cero, no generar hijos para este nodo,
// y considerar la heurística del nodo como la heurística de la jugada que representa
// (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _, _, _) :-
	simularJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica).
// Si la profundidad restante no es cero, generar hijos para este nodo del árbol
// y considerar la heurística del nodo como la heurística máxima o mínima de las jugadas
// hijas
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, MinimoMax, MaximoMin, Maximizar) :-
	Profundidad > 0 & // Para hacer esta regla exclusiva respecto de la primera
	negar(Maximizar, NuevoMaximizar) & // Para alternar entre max y min en cada nivel de profundidad del árbol
	misJugadasTeniendoCuentaMaximin(NuevoMaximizar, MisJugadas) &
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas) &
	minimaxNodoArbol(JugadaActual, Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, NuevoMaximizar).

// Los nodos que dan lugar a por lo menos una jugada posible deben de ser analizados
minimaxNodoArbol(_, Jugadas, JugadaOptima, Heuristica, ProfundidadAnterior, MinimoMax, MaximoMin, Maximizar) :-
	Jugadas \== [] &
	minimaxVariasJugadas(Jugadas, JugadaOptima, Heuristica, ProfundidadAnterior - 1, MinimoMax, MaximoMin, Maximizar).
// Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimaxNodoArbol(JugadaActual, [], JugadaActual, Heuristica, _, _, _, _) :-
	simularJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica).

// Cláusula interfaz para obtener el resultado de aplicar minimax con poda alfa-beta sobre
// un nivel del árbol de juego
minimaxVariasJugadas(Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar) :-
	heuristicaVictoria(MaximaHeuristica) &
	heuristicaDerrota(MinimaHeuristica) &
	negar(Maximizar, MaximizarProc) &
	valorOptimo(MinimaHeuristica - 1, MaximaHeuristica + 1, MaximizarProc, HeuristicaComp) &
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar, [], HeuristicaComp).
// Si no quedan jugadas a las que aplicar minimax, no tenemos nada más que hacer (caso base)
minimaxVariasJugadas_impl([], JugadaOptimaActual, HeuristicaActual, _, _, _, _, JugadaOptimaActual, HeuristicaActual).
// Mientras queden jugadas a las que aplicar minimax, aplicar minimax con poda alfa-beta
minimaxVariasJugadas_impl([Jugada|Cdr], JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	minimax_impl(Jugada, [_, HeuristicaComp], Profundidad, MinimoMax, MaximoMin, Maximizar) &
	valorOptimo(HeuristicaActual, HeuristicaComp, Maximizar, HeuristicaOpt) &
	jugadaOptima(HeuristicaActual, HeuristicaOpt, JugadaOptimaActual, Jugada, JugadaOpt) &
	valorOptimoCondicional(Maximizar, MinimoMax, HeuristicaOpt, true, NuevoMinimoMax) &
	negar(Maximizar, MaximizarNegado) &
	valorOptimoCondicional(MaximizarNegado, MaximoMin, HeuristicaOpt, false, NuevoMaximoMin) &
	podaAlfaBeta(NuevoMinimoMax, NuevoMaximoMin, Cdr, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOpt, HeuristicaOpt).

// Realiza o no la poda alfa-beta en el nivel actual del árbol de juego, dependiendo de si se cumple
// la condición de poda (MinimoMax mayor o igual que MaximoMin) o no
podaAlfaBeta(MinimoMax, MaximoMin, Jugadas, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax < MaximoMin & // No se cumple condición de poda
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual).
podaAlfaBeta(MinimoMax, MaximoMin, _, JugadaOptimaActual, HeuristicaActual, _, _, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax >= MaximoMin. // Se cumple condición de poda. No seguir analizando jugadas posibles por aquí

// Si las heurísticas a comparar difieren, entonces la última jugada que se pasa como parámetro se
// unifica con el último parámetro (nueva jugada óptima).
// En caso contrario, si son iguales, entonces el último parámetro se unifica con la primera jugada
// que se pasa como parámetro (la jugada óptima se queda como está).
jugadaOptima(HeuristicaActual, HeuristicaOpt, _, Jugada, Jugada) :-
	HeuristicaActual \== HeuristicaOpt.
jugadaOptima(HeuristicaActual, HeuristicaOpt, JugadaOptimaActual, _, JugadaOptimaActual) :-
	HeuristicaActual = HeuristicaOpt.

// Devuelve el valor apropiado de MisJugadas para la generación de jugadas, teniendo en cuenta si se está ejecutando minimax o bien maximin.
// Esencialmente, estas cláusulas hacen que resultado = MiJugada XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MisJugadas, true) :- (not MisJugadas & haciendoMaximin(true)) | (MisJugadas & not haciendoMaximin(true)).
misJugadasTeniendoCuentaMaximin(MisJugadas, false) :- (not MisJugadas & not haciendoMaximin(true)) | (MisJugadas & haciendoMaximin(true)).

// Cláusulas interfaz para generar las jugadas inmediatas a partir de una jugada que
// se considera ya hecha (aunque realmente no sea así)
// Si nadie gana, y el tablero no está lleno, entonces generar jugadas
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	simularJugada(JugadaHecha) &
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia) &
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival) &
	not VictoriaMia & not VictoriaRival & not tableroLleno &
	generarJugadasInmediatas_impl(0, JugadaHecha, difListas(JugadasGeneradas, []), MisJugadas).
// Si alguien ha ganado, o el tablero está lleno, no hay más jugadas posibles
generarJugadasInmediatas(JugadaHecha, [], _) :-
	simularJugada(JugadaHecha) &
	jugadorGano_cacheado(JugadaHecha, true, VictoriaMia) &
	jugadorGano_cacheado(JugadaHecha, false, VictoriaRival) &
	(VictoriaMia | VictoriaRival | tableroLleno).
// Si hay una columna siguiente, y tiene hueco para una ficha, entonces generar una nueva jugada con ella,
// y añadirla a la lista
generarJugadasInmediatas_impl(X, JugadaHecha, JugadasGeneradas, MisJugadas) :-
	X >= 0 & X < 8 &
	tablero(X, 0, 0) &
	generarJugadasInmediatas_impl(X + 1, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas) &
	calcularGravedad(X, Y) &
	append_simple(JugadaHecha, [movimiento(X, Y, MisJugadas)], NuevaJugada) & // No vamos a tener que iterar sobre muchas jugadas
	append_dl(difListas([NuevaJugada|Cdr], Cdr), difListas(InicioJugadasGeneradas, FinJugadasGeneradas), JugadasGeneradas).
// Si hay una columna siguiente, pero no hay hueco para una ficha, continuar iteraciones sin añadir nuevas jugadas
generarJugadasInmediatas_impl(X, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas) :-
	X >= 0 & X < 8 &
	tablero(X, 0, Jugador) &
	Jugador \== 0 &
	generarJugadasInmediatas_impl(X + 1, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas).
// Si no hay una columna siguiente, las nuevas jugadas generadas se corresponden
// con la lista vacía (caso base)
generarJugadasInmediatas_impl(X, _, difListas(JugadasGeneradas, JugadasGeneradas), _) :-
	X < 0 | X >= 8.

// Comprueba si el tablero está lleno
tableroLleno :- not tablero(_, _, 0).

// Sin jugada que aplicar, no hacer nada (caso base)
aplicarJugada([]).
// Aplicar cada uno de los movimientos
aplicarJugada([Movimiento|Cdr]) :-
	aplicarMovimiento(Movimiento) &
	aplicarJugada(Cdr).

// Simula un movimiento en el tablero
aplicarMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id) &
	.abolish(tablero(X, Y, 0)) &
	.asserta(tablero(X, Y, Id)).

// Aplica una jugada, deshaciendo cualquier otra jugada simulada anterior, para evitar incongruencias.
// En caso de que ya esté aplicada, devuelve verdadero igualmente
simularJugada(Jugada) :-
	not jugadaHecha(Jugada) &
	deshacerJugadaHecha &
	aplicarJugada(Jugada) &
	.asserta(jugadaHecha(Jugada)).
simularJugada(Jugada) :- jugadaHecha(Jugada).

// Sin jugada que deshacer, no hacer nada (caso base)
deshacerJugada([]).
// Deshacer cada uno de los movimientos
deshacerJugada([Movimiento|Cdr]) :-
	deshacerMovimiento(Movimiento) &
	deshacerJugada(Cdr).

// Deshace la simulación de un movimiento en el tablero
deshacerMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id) &
	.abolish(tablero(X, Y, Id)) &
	.asserta(tablero(X, Y, 0)).

// Deshace la jugada hecha actualmente. Si no se ha hecho una, devuelve verdadero igualmente
deshacerJugadaHecha :-
	jugadaHecha(Jugada) &
	deshacerJugada(Jugada) &
	.abolish(jugadaHecha(Jugada)).
deshacerJugadaHecha :- not jugadaHecha(_).

// Calcula la coordenada vertical donde caería una ficha colocada en la columna X
calcularGravedad(X, Y) :-
	calcularGravedad_impl(X, Y, 7).
// Si hay un hueco disponible en la Y actual, es ahí donde cae (caso base)
calcularGravedad_impl(X, YActual, YActual) :-
	YActual >= 0 & YActual < 8 &
	tablero(X, YActual, 0).
// Si no hay un hueco en la ordenada actual, pero todavía estamos en rango, seguir comprobando
calcularGravedad_impl(X, Y, YActual) :-
	YActual >= 0 & YActual < 8 &
	tablero(X, YActual, Jugador) &
	Jugador \== 0 &
	calcularGravedad_impl(X, Y, YActual - 1).

// Concatena dos listas expresadas como diferencias de listas.
// Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

// Concatena dos listas de manera trivial.
// Esta operación es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

// Obtiene el valor mínimo de dos variables instanciadas
valorMinimo(A, B, A) :- A < B.
valorMinimo(A, B, B) :- A >= B.

// Obtiene el valor máximo de dos variables instanciadas
valorMaximo(A, B, B) :- B >= A.
valorMaximo(A, B, A) :- A > B.

// Obtiene el valor óptimo de entre los dados, que puede ser el máximo o el mínimo, dependiendo de un argumento que se pase
valorOptimo(A, B, true, Maximo) :- valorMaximo(A, B, Maximo).
valorOptimo(A, B, false, Minimo) :- valorMinimo(A, B, Minimo).

// Si el primer parámetro es verdadero, Resultado se unificará con el valor óptimo entre los dos dados.
// En el caso contrario, si el primer parámetro es falso, se unificará con el valor del segundo parámetro.
valorOptimoCondicional(true, A, B, Maximizar, Resultado) :- valorOptimo(A, B, Maximizar, Resultado).
valorOptimoCondicional(false, A, _, _, A).

// Obtiene la negación de un valor de verdad en un argumento, utilizando la negación por fallo disponible en Prolog
negar(ValorVerdad, false) :- ValorVerdad.
negar(ValorVerdad, true) :- not ValorVerdad.

soyYoAIdentificadorJugador(true, Id) :-
	.my_name(Yo) &
	.delete("player", Yo, IdStr) &
	.term2string(Id, IdStr).
soyYoAIdentificadorJugador(false, 1 + (MiId mod 2)) :- soyYoAIdentificadorJugador(true, MiId).

heuristica(J, V) :- .print(J) & .random(V).

jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :-
	not jugadorGano_(Jugada, Yo, ValorVerdad) &
	jugadorGano(Yo, ValorVerdad) & // El parámetro Jugada tan solo nos interesa para distinguir entre estados del tablero y garantizar coherencia de caché, no para computar si el jugador ha ganado
	.assertz(jugadorGano_(Jugada, Yo, ValorVerdad)).
jugadorGano_cacheado(Jugada, Yo, ValorVerdad) :- jugadorGano_(Jugada, Yo, ValorVerdad).
// Borra de la base de conocimiento reglas temporales, usadas para recordar resultados parciales
borrar_jugadorGano_cacheado :- .abolish(jugadorGano_(_, _, _)).

// Cláusula interfaz para comprobar si alguien ha ganado la partida o no. El segundo argumento existe para que se pueda guardar
// en caché el valor de salida de tal argumento, en vez de si se ha encontrado una solución o no.
// Diferencia principal entre esta regla y caracteristicaRaya: detiene la evaluación de rayas al encontrar la primera de 4,
// y no sigue hasta el final del tablero, por lo que es algo más eficiente
jugadorGano(_, false).// :- jugadorGano_impl(0, 0, Yo, ValorVerdad).

/* Objetivos iniciales */

/* Planes */

// Jugar aleatoriamente
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.wait(750); // Por si estamos recibiendo todavía percepciones del tablero

	// Por si quedó otro predicado turno/1 en la BC.
	// Este .abolish pareció arreglar un extraño bug que no he podido reproducir
	// en mi ordenador, pero sí en el de Juan. Quizás se deba a alguna condición
	// de carrera. El bug hacía que el agente dejase de jugar tras el primer
	// movimiento
	.abolish(turno(_)[source(percept)]);

	// Pedir paciencia :)
	.print("Deliberando movimiento... Por favor, espera.");

	// Dependiendo de la estrategia a emplear, escoger el mejor o peor movimiento
	// (desde nuestro punto de vista)
	?estrategia(Est);
	if (Est = jugarAGanar) {
		?mejorSiguienteColumna(X);
	} else {
		?peorSiguienteColumna(X);
	};

	// Realizar el movimiento a esa casilla
	put(X).

// Descartar comunicaciones que lleguen de otros agentes, pues solo nos interesa
// lo que diga el entorno
+!kqml_received(Agente, _, _, _) : .my_name(Yo) & Agente \== Yo.
