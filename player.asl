// Agente player.asl en proyecto ConnectAlfa.mas2j

/* Creencias y reglas iniciales */

// El valor devuelto por la heurística para señalar una victoria. Es el valor
// máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
// El valor devuelto por la heurística para señalar una derrota. Es el valor
// mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).
// El número de entradas actuales en la tabla de transposiciones
entradasMapaZobrist(0).

// Cláusulas interfaz para obtener la columna donde colocar una ficha para maximizar
// nuestra victoria o la del contrincante, dado un nivel de profundidad
mejorSiguienteColumna :-
	profundidadBusqueda(P) &
	minimax([[movimiento(X, _, _)|_], _], P) &
	.abolish(movimientoOptimo(_)) &
	.asserta(movimientoOptimo(X)).
peorSiguienteColumna :-
	profundidadBusqueda(P) &
	maximin([[movimiento(X, _, _)|_], _], P) &
	.abolish(movimientoOptimo(_)) &
	.asserta(movimientoOptimo(X)).

// Cláusula interfaz para obtener la jugada óptima a realizar, con su heurística asociada
minimax(JugadaYHeuristica, P) :-
	heuristicaDerrota(MinimoMax) & // MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo
	heuristicaVictoria(MaximoMin) & // MaximoMin = Beta
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, false) &
	// Borrar datos en caché y simulaciones temporales
	deshacerJugadaHecha.
// Cláusula interfaz para obtener la jugada óptima a realizar para perder en el juego, con su heurística asociada
maximin(JugadaYHeuristica, P) :-
	heuristicaDerrota(MinimoMax) & // MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo que una letra griega
	heuristicaVictoria(MaximoMin) & // MaximoMin = Beta
	.asserta(haciendoMaximin(true)) & // Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, true) &
	.abolish(haciendoMaximin(_)) &
	// Borrar datos en caché y simulaciones temporales
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
	consultarTablaTransposicion(JugadaActual, Profundidad, MinimoMax, MaximoMin, NuevoMinimoMax, NuevoMaximoMin, JugadaOptimaLeida, HeuristicaLeida) &
	unificarCon(NuevoMinimoMax, MinimoMax) &
	unificarCon(NuevoMaximoMin, MaximoMin) &
	unificarCon(JugadaOptima, JugadaOptimaLeida) &
	unificarCon(Heuristica, HeuristicaLeida) &
	negar(Maximizar, NuevoMaximizar) & // Para alternar entre max y min en cada nivel de profundidad del árbol
	seguirMinimaxSiNecesario(JugadaActual, [JugadaOptima, Heuristica], Profundidad, NuevoMinimoMax, NuevoMaximoMin, NuevoMaximizar).

// Sigue ejecutando minimax si y solo si lo leído de la tabla de transposiciones lo aconseja
seguirMinimaxSiNecesario(JugadaActual, [JugadaOptima, Heuristica], Profundidad, MinimoMax, MaximoMin, Maximizar) :-
	not .ground(JugadaOptima) & not .ground(Heuristica) &
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas) &
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas) &
	minimaxNodoArbol(JugadaActual, Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar).
seguirMinimaxSiNecesario(_, [JugadaOptima, Heuristica], _, _, _, _) :-
	.ground(JugadaOptima) & .ground(Heuristica). // Hemos leído los datos necesarios de la tabla, así que no hacer minimax

// Consulta la tabla de transposición, obteniendo de ella cotas inferiores y superiores,
// así como la jugada más óptima en respuesta a la actual, si se ha determinado
consultarTablaTransposicion(Jugada, Profundidad, MinimoMax, MaximoMin, NuevoMinimoMax, NuevoMaximoMin, JugadaOptima, Heuristica) :-
	simularJugada(Jugada) &
	enMapaZobrist &
	obtenerEntradaZobrist([ JugadaOptimaE, HeuristicaE, Tipo, ProfundidadE ]) &
	(ProfundidadE < (Profundidad - 1) | // No se evalúa el segundo operando si el primero es cierto, pero añade un punto de regreso para obtener más soluciones
	procesarEntradaTablaTrans(Tipo, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin, JugadaOptima, Heuristica, NuevoMinimoMax, NuevoMaximoMin)).
consultarTablaTransposicion(Jugada, _, MinimoMax, MaximoMin, MinimoMax, MaximoMin, _, _) :-
	simularJugada(Jugada) &
	not enMapaZobrist.

// Predicados que leen de la entrada de la tabla de transposiciones las variables correspondientes
procesarEntradaTablaTrans(exacto, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin).
procesarEntradaTablaTrans(cotaInferior, _, Valor, MinimoMax, MaximoMin, _, _, NuevoMinimoMax, MaximoMin) :-
	valorMaximo(MinimoMax, Valor, NuevoMinimoMax).
procesarEntradaTablaTrans(cotaSuperior, _, Valor, MinimoMax, MaximoMin, _, _, MinimoMax, NuevoMaximoMin) :-
	valorMinimo(MaximoMin, Valor, NuevoMaximoMin).

// Los nodos que dan lugar a por lo menos una jugada posible deben de ser analizados
minimaxNodoArbol(JugadaActual, Jugadas, JugadaOptima, Heuristica, ProfundidadAnterior, MinimoMax, MaximoMin, Maximizar) :-
	Jugadas \== [] &
	ordenarJugadas(JugadaActual, Jugadas, JugadasOrdenadas, ProfundidadAnterior - 1) &
	minimaxVariasJugadas(JugadaActual, JugadasOrdenadas, JugadaOptima, Heuristica, ProfundidadAnterior - 1, MinimoMax, MaximoMin, Maximizar).
// Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimaxNodoArbol(JugadaActual, [], JugadaActual, Heuristica, _, _, _, _) :-
	simularJugada(JugadaActual) &
	heuristica(JugadaActual, Heuristica).

// Coloca como primera jugada aquella que hemos determinado que es el mejor movimiento actual
// Primera regla comentada porque se desactivó la poda alfa-beta, y al no usarla no tiene sentido ordenar
// jugadas. Las razones para ello se detallan en un comentario más abajo
/*ordenarJugadas(JugadaActual, Jugadas, [JugadaOptima|JugadasProc], _) :-
	simularJugada(JugadaActual) &
	enMapaZobrist &
	obtenerEntradaZobrist([ JugadaOptima, _, exacto, _ ]) &
	eliminarElementoLista(Jugadas, JugadaOptima, JugadasProc).*/
ordenarJugadas(_, Jugadas, Jugadas, _).

// Cláusula interfaz para obtener el resultado de aplicar minimax con poda alfa-beta sobre
// un nivel del árbol de juego
minimaxVariasJugadas(JugadaActual, Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar) :-
	heuristicaVictoria(MaximaHeuristica) &
	heuristicaDerrota(MinimaHeuristica) &
	negar(Maximizar, MaximizarProc) &
	valorOptimo(MinimaHeuristica - 1, MaximaHeuristica + 1, MaximizarProc, HeuristicaComp) &
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar, [], HeuristicaComp) &
	actualizarTablaTransposiciones(JugadaActual, JugadaOptima, Heuristica, MinimoMax, MaximoMin, NuevaProfundidad).
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
/*podaAlfaBeta(MinimoMax, MaximoMin, Jugadas, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax < MaximoMin & // No se cumple condición de poda
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual).
podaAlfaBeta(MinimoMax, MaximoMin, _, JugadaOptimaActual, HeuristicaActual, _, _, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax >= MaximoMin. // Se cumple condición de poda. No seguir analizando jugadas posibles por aquí*/

// Por alguna razón el uso de poda alfa-beta afecta a los resultados, y no debería. Quizás alfa y/o beta tomen valores incorrectos en alguna parte,
// pero no tengo tiempo para investigar la causa raíz, así que he desactivado la poda.
// La ausencia de poda también ayuda a que el tiempo de análisis sea más uniforme entre jugadas, pues el número de nodos procesados es más
// fácil de predecir, así que no hay mal que por bien no venga
podaAlfaBeta(MinimoMax, MaximoMin, Jugadas, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual).

// Actualiza la tabla de transposiciones con los valores deseables
actualizarTablaTransposiciones(JugadaActual, JugadaOptima, Heuristica, MinimoMax, MaximoMin, Profundidad) :-
	deducirTipoEntrada(Heuristica, MinimoMax, MaximoMin, Tipo) &
	simularJugada(JugadaActual) &
	asociarEntradaEstadoActual([ JugadaOptima, Heuristica, Tipo, Profundidad ]).

// Obtiene el tipo de entrada a guardar en la tabla de transposiciones, para actualizarTablaTransposiciones/6
deducirTipoEntrada(Heuristica, MinimoMax, _, cotaSuperior) :-
	MinimoMax >= Heuristica.
deducirTipoEntrada(Heuristica, _, MaximoMin, cotaInferior) :-
	Heuristica >= MaximoMin.
deducirTipoEntrada(Heuristica, MinimoMax, MaximoMin, exacto) :-
	Heuristica < MaximoMin & Heuristica > MinimoMax.

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
	jugadorGano(true, VictoriaMia) &
	jugadorGano(false, VictoriaRival) &
	not VictoriaMia & not VictoriaRival & not tableroLleno &
	generarJugadasInmediatas_impl(0, JugadaHecha, difListas(JugadasGeneradas, []), MisJugadas).
// Si alguien ha ganado, o el tablero está lleno, no hay más jugadas posibles
generarJugadasInmediatas(JugadaHecha, [], _) :-
	simularJugada(JugadaHecha) &
	jugadorGano(true, VictoriaMia) &
	jugadorGano(false, VictoriaRival) &
	(VictoriaMia | VictoriaRival | tableroLleno).
// Si hay una columna siguiente, y tiene hueco para una ficha, entonces generar una nueva jugada con ella,
// y añadirla a la lista
generarJugadasInmediatas_impl(X, JugadaHecha, JugadasGeneradas, MisJugadas) :-
	X >= 0 & X < 8 &
	tablero(X, 0, 0) &
	generarJugadasInmediatas_impl(X + 1, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas) &
	calcularGravedad(X, Y) &
	.concat(JugadaHecha, [movimiento(X, Y, MisJugadas)], NuevaJugada) & // No vamos a tener que iterar sobre muchas jugadas
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
	.abolish(tablero(X, Y, 0)[source(percept)]) &
	.asserta(tablero(X, Y, Id)[source(percept)]) &
	addRayasJugador(X, Y, Id) &
	colocarFichaZobrist(X, Y, 0, Id).

// Aplica una jugada, deshaciendo cualquier otra jugada simulada anterior, para evitar incongruencias.
// En caso de que ya esté aplicada, devuelve verdadero igualmente
simularJugada(Jugada) :-
	not jugadaHecha(Jugada) &
	deshacerJugadaHecha &
	.asserta(jugadaHecha(Jugada)) &
	aplicarJugada(Jugada).
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
	.abolish(tablero(X, Y, Id)[source(percept)]) &
	.asserta(tablero(X, Y, 0)[source(percept)]) &
	colocarFichaZobrist(X, Y, Id, 0).

// Deshace la jugada hecha actualmente. Si no se ha hecho una, devuelve verdadero igualmente
deshacerJugadaHecha :-
	jugadaHecha(Jugada) &
	deshacerJugada(Jugada) &
	.abolish(rayaSimulada(_, _, _, _, _, _)) &
	.abolish(jugadaHecha(Jugada)).
deshacerJugadaHecha :- not jugadaHecha(_).

// Calcula la coordenada vertical donde caería una ficha colocada en la columna X
calcularGravedad(X, 7 - (8 - CasillasLibres)) :-
	.count(tablero(X, _, 0), CasillasLibres).
calcularGravedad(X, 0).

// Concatena dos listas expresadas como diferencias de listas.
// Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

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

// Predicados que unifican una variable libre con otra, que también puede ser libre.
// Si la variable no es libre, no hacen nada
unificarCon(VariableLibre, Valor) :-
	not .ground(VariableLibre) &
	VariableLibre = Valor.
unificarCon(VariableLibre, _) :-
	.ground(VariableLibre).

// Predicados que eliminan las ocurrencias de un elemento en una lista cerrada
// La lista vacía no tiene nada que eliminar (caso base)
eliminarElementoLista([], _, []).
eliminarElementoLista([Car|Cdr], E, [Car|R]) :-
	Car \== E & // Un elemento diferente de aquel a eliminar se incluye en el resultado
	eliminarElementoLista(Cdr, E, R).
eliminarElementoLista([Car|Cdr], E, R) :-
	Car = E & // El elemento a eliminar no se incluye en el resultado
	eliminarElementoLista(Cdr, E, R).

// Predicados que devuelven cierto si una lista tiene algún elemento en común con otra
elementoComun([Car|_], L2) :- .member(Car, L2).
elementoComun([Car|Cdr], L2) :-
	not .member(Car, L2) &
	elementoComun(Cdr, L2).

// Predicados que obtienen el último elemento de una lista cerrada
ultimoElemento([E|[]], E).
ultimoElemento([_|Cdr], E) :-
	Cdr \== [] &
	ultimoElemento(Cdr, E).

soyYoAIdentificadorJugador(true, Id) :-
	.my_name(Yo) &
	.delete("player", Yo, IdStr) &
	.term2string(Id, IdStr).
soyYoAIdentificadorJugador(false, 1 + (MiId mod 2)) :- soyYoAIdentificadorJugador(true, MiId).

identificadorJugadorASoyYo(Id, true) :-
	soyYoAIdentificadorJugador(true, IdYo) &
	Id = IdYo.
identificadorJugadorASoyYo(Id, false) :-
	soyYoAIdentificadorJugador(false, IdOtro) &
	Id = IdOtro.

// Predicado que es verdadero si y solo si es el primer turno del juego
// (es decir, alguien ha colocado una ficha)
esPrimerTurno :- not (tablero(_, _, Id) & Id \== 0).

// Predicado que es verdadero si y solo si es una nueva partida
// (es decir, es el primer turno, o solo hay una ficha colocada)
esNuevaPartida :-
	esPrimerTurno |
	.count((tablero(_, _, Id) & Id \== 0), 1).

// Inicializa el estado anterior del tablero, que se corresponde con el estado inicial,
// si ha empezado una nueva partida.
// El tablero cambia de estado una sola vez cuando termina el turno de un jugador
inicializarTableroAnteriorSiNecesario :-
	.print("Inicializando tablero anterior...") &
	inicializarTableroSiNuevaPartida.

inicializarTableroSiNuevaPartida :-
	not esNuevaPartida &
	.print("No procede inicializar el tablero anterior, porque no es una nueva partida").
inicializarTableroSiNuevaPartida :-
	esNuevaPartida &
	.abolish(tableroAnterior(_, _, _)) &
	inicializarTableroAnterior_impl(0, 0) &
	.print("Inicializado tablero anterior").

inicializarTableroAnterior_impl(X, Y) :-
	X < 8 & Y < 8 &
	.assertz(tableroAnterior(X, Y, 0)) &
	inicializarTableroAnterior_impl(X + 1, Y).
// Si hemos terminado de procesar la fila actual, ir con la siguiente
inicializarTableroAnterior_impl(X, Y) :-
	X = 8 & Y < 7 &
	inicializarTableroAnterior_impl(0, Y + 1).
// Si hemos terminado de procesar todo el tablero, no hay más trabajo que hacer aquí
inicializarTableroAnterior_impl(X, Y) :-
	X = 8 & Y = 7.

// Obtiene el movimiento que se ha realizado desde el estado anterior del tablero
movimientoRealizado(X, Y, IdNuevo) :-
	tableroAnterior(X, Y, IdPrevio) &
	tablero(X, Y, IdNuevo) &
	IdPrevio \== IdNuevo.

// Actualiza el estado anterior del tablero con un nuevo movimiento
actualizarEstadoAnterior(X, Y, Id) :-
	.abolish(tableroAnterior(X, Y, _)) &
	.assertz(tableroAnterior(X, Y, Id)).

// Si no es el primer turno del juego, actualizar tabla de transposiciones e información del estado anterior
percibirMovimientoRival :-
	.print("Percibiendo movimiento del rival...") &
	not esPrimerTurno &
	movimientoRealizado(X, Y, Id) &
	tableroAnterior(X, Y, AnteriorId) &
	actualizarEstadoAnterior(X, Y, Id) &
	colocarFichaZobrist(X, Y, AnteriorId, Id) &
	addRayasJugador(X, Y, Id) &
	.print("El rival ha colocado una ficha en (", X, ", ", Y, ")").
// Si es el primer turno del juego, no hay nada que percibir
percibirMovimientoRival :- esPrimerTurno & .print("Nada que percibir, es el primer turno").

// Añade incrementalmente las rayas que ha realizado un jugador, a partir de la colocación de
// su última ficha
addRayasJugador(X, Y, Jugador) :-
	addRayasJugador_impl(X, Y, Jugador, -1, 0) &
	addRayasJugador_impl(X, Y, Jugador, -1, -1) &
	addRayasJugador_impl(X, Y, Jugador, 0, -1) &
	addRayasJugador_impl(X, Y, Jugador, 1, -1) &
	addRayasJugador_impl(X, Y, Jugador, 1, 0) &
	addRayasJugador_impl(X, Y, Jugador, 1, 1) &
	addRayasJugador_impl(X, Y, Jugador, 0, 1) &
	addRayasJugador_impl(X, Y, Jugador, -1, 1).
addRayasJugador_impl(X, Y, Jugador, DX, DY) :-
	calcularComienzoRaya(X, Y, Jugador, DX, DY, CX, CY, Longitud) &
	identificadorJugadorASoyYo(Jugador, MiRaya) &
	recordarRaya(CX, CY, Longitud, DX, DY, MiRaya).

// Añade a la base de conocimiento información sobre una raya, si procede
recordarRaya(CX, CY, Longitud, DX, DY, MiRaya) :-
	Longitud > 1 &
	purgarRayasRedundantes(CX, CY, Longitud, DX, DY) &
	anadirRayaABC(CX, CY, Longitud, DX, DY, MiRaya).
recordarRaya(_, _, Longitud, _, _, _) :- Longitud < 2.

// Añade información de la formación de una raya a la BC de manera diferente
// dependiendo de si estamos en un contexto de simulación de jugadas o no
anadirRayaABC(CX, CY, Longitud, DX, DY, MiRaya) :-
	jugadaHecha(_) &
	.assertz(rayaSimulada(CX, CY, Longitud, DX, DY, MiRaya)).
anadirRayaABC(CX, CY, Longitud, DX, DY, MiRaya) :-
	not jugadaHecha(_) &
	.assertz(raya(CX, CY, Longitud, DX, DY, MiRaya)).

// Cláusula interfaz para calcular dónde comienza una raya de un jugador
calcularComienzoRaya(X, Y, Jugador, DX, DY, CX, CY, Longitud) :-
	calcularComienzoRaya_impl(Jugador, DX, DY, CX, CY, Longitud, X, Y, 0).
// Seguir recorriendo la el tablero en la dirección de la posible raya hasta que se agote
calcularComienzoRaya_impl(Jugador, DX, DY, CX, CY, Longitud, XActual, YActual, LongitudActual) :-
	XActual >= 0 & XActual < 8 & YActual >= 0 & YActual < 8 &
	tablero(XActual, YActual, Jugador) &
	(LongitudActual + 1) < 5 &
	calcularComienzoRaya_impl(Jugador, DX, DY, CX, CY, Longitud, XActual - DX, YActual - DY, LongitudActual + 1).
// Si hemos encontrado una casilla que no tiene una ficha del jugador, o nos salimos del tablero,
// la raya se detiene aquí (caso base)
calcularComienzoRaya_impl(Jugador, DX, DY, XActual + DX, YActual + DY, LongitudActual, XActual, YActual, LongitudActual) :-
	XActual < 0 | XActual > 7 | YActual < 0 | YActual > 7 |
	(tablero(XActual, YActual, Id) & Id \== Jugador).
// Si llegamos a 5 fichas o más, limitarnos a considerar la raya de 4 ya formada
calcularComienzoRaya_impl(Jugador, DX, DY, XActual + DX, YActual + DY, 4, XActual, YActual, LongitudActual) :-
	XActual >= 0 & XActual < 8 & YActual >= 0 & YActual < 8 &
	tablero(XActual, YActual, Jugador) &
	(LongitudActual + 1) >= 5.

// Elimina de la BC las rayas que ocupan alguna de las casillas de otra raya, en la misma dirección
// (pero en igual u opuesto sentido)
purgarRayasRedundantes(CX, CY, Longitud, DX, DY) :-
	casillasOcupadas(CX, CY, Longitud, DX, DY, Casillas) &
	eliminarRayasQueOcupanAlgunaCasilla(Casillas, DX, DY) &
	eliminarRayasQueOcupanAlgunaCasilla(Casillas, -DX, -DY).

// Una raya de longitud 0 no ocupa ninguna casilla (caso base)
casillasOcupadas(_, _, 0, _, _, []).
// Mientras continúe la raya, añadir las casillas que ocupa a la lista resultado
casillasOcupadas(X, Y, LongitudRestante, DX, DY, [casilla(X, Y)|Casillas]) :-
	LongitudRestante > 0 &
	casillasOcupadas(X + DX, Y + DY, LongitudRestante - 1, DX, DY, Casillas).

// Cláusula interfaz para eliminar de la BC las rayas que ocupan alguna de las casillas dadas,
// con una dirección determinada
eliminarRayasQueOcupanAlgunaCasilla(Casillas, DX, DY) :-
	.findall(raya(CX, CY, Longitud, DX, DY, MiRaya), raya(CX, CY, Longitud, DX, DY, MiRaya), Rayas) &
	eliminarRayasQueOcupanAlgunaCasilla_impl(Rayas, Casillas).
// No hay nada que hacer si no hay rayas en la BC (caso base)
eliminarRayasQueOcupanAlgunaCasilla_impl([], _).
// Si la raya ocupa alguna casilla dada, eliminarla y analizar la siguiente
eliminarRayasQueOcupanAlgunaCasilla_impl([raya(CX, CY, Longitud, DX, DY, MiRaya)|Cdr], Casillas) :-
	ocupaRayaAlgunaCasilla(CX, CY, Longitud, DX, DY, Casillas) &
	.abolish(raya(CX, CY, Longitud, DX, DY, MiRaya)) &
	eliminarRayasQueOcupanAlgunaCasilla_impl(Cdr, Casillas).
// Si la raya no ocupa alguna casilla dada, solo ir a la siguiente
eliminarRayasQueOcupanAlgunaCasilla_impl([raya(CX, CY, Longitud, DX, DY, _)|Cdr], Casillas) :-
	not ocupaRayaAlgunaCasilla(CX, CY, Longitud, DX, DY, Casillas) &
	eliminarRayasQueOcupanAlgunaCasilla_impl(Cdr, Casillas).

// Predicado que es cierto si y solo si una raya ocupa alguna de las casillas dadas
ocupaRayaAlgunaCasilla(CX, CY, Longitud, DX, DY, Casillas) :-
	casillasOcupadas(CX, CY, Longitud, DX, DY, Ocupadas) &
	elementoComun(Casillas, Ocupadas).

// Elimina de la base de conocimientos los datos deducidos sobre las rayas formadas
olvidarRayasSiNuevaPartida :- not esNuevaPartida.
olvidarRayasSiNuevaPartida :-
	esNuevaPartida &
	.abolish(raya(_, _, _, _)).

// Predicados que obtienen la puntuación heurística del estado actual del tablero
// Si yo he ganado, la heurística será la máxima
heuristica(_, Valor) :-
	jugadorGano(true, true) &
	heuristicaVictoria(Valor).
// Si el contrincante ha ganado, la heurística será la mínima
heuristica(_, Valor) :-
	jugadorGano(false, true) &
	heuristicaDerrota(Valor).
// En otro caso (nadie ha ganado), la heurística se calculará en base a una función ponderada lineal
heuristica(_, Valor) :-
	jugadorGano(true, false) &
	jugadorGano(false, false) &
	heuristicaPonderadaLineal(Valor).

// Calcula una puntuación heurística a partir de características del tablero que se consideran positivas (y negativas)
heuristicaPonderadaLineal(30 * CaracteristicaImpedirRaya3 + 30 * CaracteristicaRaya3Mia + 20 * CaracteristicaRaya2Mia + CaracteristicaFichasCentroYo) :-
	caracteristicaImpedirRaya(true, CaracteristicaImpedirRaya3, 3) &
	caracteristicaRaya(true, CaracteristicaRaya3Mia, 3) &
	caracteristicaRaya(true, CaracteristicaRaya2Mia, 2) &
	caracteristicaFichasEnCentro(true, CaracteristicaFichasCentroYo).

// Computa la característica de formar una raya de N fichas
caracteristicaRaya(Yo, CaracteristicaRaya, Fichas) :-
	.count(raya(_, _, Fichas, _, _, Yo), CaracteristicaRaya).

// Cláusula interfaz para computar el valor de la característica de impedir la formación de una raya al rival
caracteristicaImpedirRaya(_, 0, 0).
caracteristicaImpedirRaya(Yo, CaracteristicaImpedirRaya, Fichas) :-
	(Fichas - 1) > 0 &
	negar(Yo, Otro) &
	.findall(raya(CX, CY, Fichas - 1, DX, DY, Otro), rayaSimulada(CX, CY, Fichas - 1, DX, DY, Otro), Rayas) &
	caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, Fichas - 1, Rayas, 0).
// Sin más rayas que procesar, el valor de la heurística se queda como está (caso base)
caracteristicaImpedirRaya_impl(_, CaracteristicaImpedirRaya, _, [], CaracteristicaImpedirRaya).
// Mientras queden rayas que procesar, incrementar la heurística si tenemos una ficha en una posición que bloquee esa raya
caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, Fichas, [raya(CX, CY, Fichas, DX, DY, _)|Cdr], CaracteristicaActual) :-
	casillasOcupadas(CX, CY, Fichas, DX, DY, Casillas) &
	ultimoElemento(Casillas, casilla(X, Y)) &
	incrementarSiImpide(Yo, X + DX, Y + DY, CaracteristicaActual, NuevaCaracteristica) &
	caracteristicaImpedirRaya_impl(Yo, CaracteristicaImpedirRaya, Fichas, Cdr, NuevaCaracteristica).

// Predicados que incrementan el valor de CaracteristicaActual dependiendo de si se cumple la condición
// de bloqueo o no
incrementarSiImpide(Yo, XBloq, YBloq, CaracteristicaActual, CaracteristicaActual) :-
	soyYoAIdentificadorJugador(Yo, Id) &
	not tablero(XBloq, YBloq, Id).
incrementarSiImpide(Yo, XBloq, YBloq, CaracteristicaActual, CaracteristicaActual + 1) :-
	soyYoAIdentificadorJugador(Yo, Id) &
	tablero(XBloq, YBloq, Id).

// Computa la característica de tener fichas en el centro
caracteristicaFichasEnCentro(Yo, CaracteristicaFichasCentro1 + CaracteristicaFichasCentro2) :-
	soyYoAIdentificadorJugador(Yo, Id) &
	.count(tablero(3, _, Id), CaracteristicaFichasCentro1) &
	.count(tablero(4, _, Id), CaracteristicaFichasCentro2).

// Cláusulas que comprueban si un jugador ha ganado; es decir, ha formado una raya de 4
jugadorGano(Yo, false) :- not rayaSimulada(_, _, 4, _, _, Yo).
jugadorGano(Yo, true) :- rayaSimulada(_, _, 4, _, _, Yo).

// Calcula el primer valor del hash de Zobrist para el estado inicial del tablero.
// Implementación basada en https://en.wikipedia.org/wiki/Zobrist_hashing
inicializarZobrist :-
	inicializarTablasZobrist &
	hashZobrist(Hash) &
	.asserta(hashZobristActual(Hash)).

// Crea las estructuras de datos necesarias para utilizar la función de
// dispersión Zobrist
inicializarTablasZobrist :-
	.print("Inicializando tablas de función de dispersión Zobrist...") &
	inicializarTablasZobrist_impl(0, 0, 0).
inicializarTablasZobrist_impl(X, Y, Id) :-
	X < 8 & Y < 8 & Id < 3 &
	esei.si.alejandrogg.claveAleatoria(Num) &
	.assertz(tablaZobrist(X, Y, Id, Num)) &
	inicializarTablasZobrist_impl(X, Y, Id + 1).
// Si no hay más posibles jugadas en la casilla actual, moverse a la siguiente
inicializarTablasZobrist_impl(X, Y, Id) :-
	X < 8 & Y < 8 & Id = 3 &
	inicializarTablasZobrist_impl(X + 1, Y, 0).
// Si no hay más posibles jugadas en la fila actual, moverse a la siguiente
inicializarTablasZobrist_impl(X, Y, _) :-
	X = 8 & Y < 7 &
	inicializarTablasZobrist_impl(0, Y + 1, 0).
// Si hemos terminado de recorrer las posibles entradas, terminar la recursividad
inicializarTablasZobrist_impl(X, Y, _) :-
	.print("Tablas de la función de dispersión Zobrist inicializadas") &
	X = 8 & Y = 7.

// Calcula el valor de dispersión (hash) del estado actual del tablero
hashZobrist(Hash) :-
	.print("Calculando valor de dispersión Zobrist inicial...") &
	hashZobrist_impl(0, 0, Hash, 0).
hashZobrist_impl(X, Y, Hash, HashActual) :-
	X < 8 & Y < 8 &
	tablero(X, Y, Id) &
	tablaZobrist(X, Y, Id, Num) &
	esei.si.alejandrogg.bitXor(HashActual, Num, NuevoHash) &
	hashZobrist_impl(X + 1, Y, Hash, NuevoHash).
// Si hemos terminado de procesar la fila actual, ir con la siguiente
hashZobrist_impl(X, Y, Hash, HashActual) :-
	X = 8 & Y < 7 &
	hashZobrist_impl(0, Y + 1, Hash, HashActual).
// Si hemos terminado de procesar todo el tablero, tenemos el hash final
hashZobrist_impl(X, Y, Hash, Hash) :-
	.print("Valor de dispersión Zobrist inicial calculado") &
	X = 8 & Y = 7.

// Actualiza el hash Zobrist calculado con una colocación de una ficha en el tablero
colocarFichaZobrist(X, Y, AnteriorId, NuevoId) :-
	hashZobristActual(Hash) &
	tablaZobrist(X, Y, AnteriorId, AnteriorNum) &
	esei.si.alejandrogg.bitXor(Hash, AnteriorNum, HashProcAux) &
	tablaZobrist(X, Y, NuevoId, NuevoNum) &
	esei.si.alejandrogg.bitXor(HashProcAux, NuevoNum, HashProc) &
	.abolish(hashZobristActual(_)) &
	.asserta(hashZobristActual(HashProc)).

// Crea una entrada en la tabla de transposiciones para el estado actual.
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas) &
	Entradas < 65536 & // A 1 KiB por entrada (estimación a ojo de buen cubero pesimista), las entradas ocuparían 65 MiB
	eliminarDeMapaSiEsta &
	hashZobristActual(Hash) &
	.abolish(entradasMapaZobrist(_)) &
	.asserta(entradasMapaZobrist(Entradas + 1)) &
	.assertz(mapaZobrist(Hash, E)).
// Si llegamos al máximo de entradas, vaciar el mapa y volver a empezar
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas) &
	Entradas >= 65536 &
	esei.si.alejandrogg.retract(mapaZobrist(_, _)) &
	hashZobristActual(Hash) &
	.assertz(mapaZobrist(Hash, E)).

// Elimina la entrada en la tabla correspondiente al estado actual, si hay
eliminarDeMapaSiEsta :-
	enMapaZobrist &
	hashZobristActual(Hash) &
	entradasMapaZobrist(Entradas) &
	.abolish(entradasMapaZobrist(_)) &
	.asserta(entradasMapaZobrist(Entradas - 1)) &
	.abolish(mapaZobrist(Hash, _)).
eliminarDeMapaSiEsta :-
	not enMapaZobrist.

// Comprueba si hay una entrada para el estado actual del tablero en la tabla de
// transposiciones
enMapaZobrist :-
	hashZobristActual(Hash) &
	mapaZobrist(Hash, _).

// Obtiene la entrada asociada al estado actual del tablero en la tabla de
// transposiciones. No maneja colisiones, que se estiman muy poco probables
obtenerEntradaZobrist(E) :-
	hashZobristActual(Hash) &
	mapaZobrist(Hash, E).

// Vacía la tabla de transposiciones
vaciarMapaZobrist :-
	.abolish(mapaZobrist(_, _)) &
	.abolish(entradasMapaZobrist(_)) &
	.asserta(entradasMapaZobrist(0)).

/* Objetivos iniciales */

!inicializarEstructurasDatos.

/* Planes */

// Inicializa las estructuras de datos del agente
+!inicializarEstructurasDatos[source(self)] : inicializarZobrist.

// Analiza y realiza la mejor jugada decidible para el estado actual del tablero
+!hacerMejorJugada[source(self)] :
	estrategia(Est) & esei.si.alejandrogg.segundosJugadas(TiempoMax) &
	vaciarMapaZobrist &
	olvidarRayasSiNuevaPartida & inicializarTableroAnteriorSiNecesario &
	percibirMovimientoRival & soyYoAIdentificadorJugador(true, MiId)
<-
	+inicioAnalisis(system.time);
	?inicioAnalisis(Inicio);
	+profundidadBusqueda(1);

	// Realizar iterative deepening hasta que se agote el tiempo, o hayamos analizado la jugada a bastante profundidad
	while (profundidadBusqueda(P) & system.time - Inicio < TiempoMax * 1000 & P <= 8) {
		.print("Búsqueda de profundidad ", P, " en curso... (", system.time - Inicio, " ms desde profundidad anterior)");
		if (Est = jugarAGanar) {
			?mejorSiguienteColumna;
		} else {
			?peorSiguienteColumna;
		};

		-+profundidadBusqueda(P + 1);
	};

	// Mostrar cuánto hemos tardado
	?movimientoOptimo(X);
	.print("Movimiento a ", X, " analizado en ", system.time - Inicio, " ms");

	-inicioAnalisis(_);
	-profundidadBusqueda(_);
	-movimientoOptimo(_);

	// Recordar rayas que pudimos haber hecho, y calcular nuevo estado anterior
	?calcularGravedad(X, Y);
	-+tablero(X, Y, MiId)[source(percept)]; // Considerar el movimiento hecho, para que los predicados funcionen correctamente
	?addRayasJugador(X, Y, MiId);
	?actualizarEstadoAnterior(X, Y, MiId);

	// Finalmente, realizar el movimiento a la columna calculada
	put(X).

// Reaccionar al evento de turno recibido
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.print("Turno recibido. Esperando por inicialización de estructuras de datos...");

	// Esperar a que se inicialicen todas las variables, si es necesario.
	// Esta espera devuelve el control inmediatamente si las creencias están en la BC
	.wait(hashZobristActual(_));

	.wait(1250); // Por si estamos recibiendo todavía percepciones del tablero

	// Por si quedó otro predicado turno/1 en la BC.
	// Este .abolish pareció arreglar un extraño bug que no he podido reproducir
	// en mi ordenador, pero sí en el de Juan Carlos. Quizás se deba a alguna condición
	// de carrera. El bug hacía que el agente dejase de jugar tras el primer
	// movimiento
	.abolish(turno(_)[source(percept)]);

	.print("Estructuras de datos inicializadas. Analizando jugadas...");

	!hacerMejorJugada.

// Descartar comunicaciones que lleguen de otros agentes, pues solo nos interesa
// lo que diga el entorno
+!kqml_received(Agente, _, _, _) : .my_name(Yo) & Agente \== Yo.
