// Agente player.asl en proyecto ConnectAlfa.mas2j

/* Creencias y reglas iniciales */

// El valor devuelto por la heurística para señalar una victoria. Es el valor
// máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
// El valor devuelto por la heurística para señalar una derrota. Es el valor
// mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).

// Cláusulas interfaz para obtener la columna donde colocar una ficha para maximizar
// nuestra victoria o la del contrincante, dado un nivel de profundidad
mejorSiguienteColumna :-
	profundidadBusqueda(P) &
	minimax([[movimiento(X, _, _)|_], _], P) &
	.asserta(movimientoOptimo(X)).
peorSiguienteColumna :-
	profundidadBusqueda(P) &
	maximin([[movimiento(X, _, _)|_], _], P) &
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
ordenarJugadas(JugadaActual, Jugadas, [JugadaOptima|JugadasProc], _) :-
	simularJugada(JugadaActual) &
	enMapaZobrist &
	obtenerEntradaZobrist([ JugadaOptima, _, exacto, _ ]) &
	eliminarElementoLista(Jugadas, JugadaOptima, JugadasProc).
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
podaAlfaBeta(MinimoMax, MaximoMin, Jugadas, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax < MaximoMin & // No se cumple condición de poda
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual).
podaAlfaBeta(MinimoMax, MaximoMin, _, JugadaOptimaActual, HeuristicaActual, _, _, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax >= MaximoMin. // Se cumple condición de poda. No seguir analizando jugadas posibles por aquí

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
	.abolish(tablero(X, Y, 0)[source(percept)]) &
	.asserta(tablero(X, Y, Id)[source(percept)]) &
	colocarFichaZobrist(X, Y, 0, Id).

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
	.abolish(tablero(X, Y, Id)[source(percept)]) &
	.asserta(tablero(X, Y, 0)[source(percept)]) &
	colocarFichaZobrist(X, Y, Id, 0).

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

soyYoAIdentificadorJugador(true, Id) :-
	.my_name(Yo) &
	.delete("player", Yo, IdStr) &
	.term2string(Id, IdStr).
soyYoAIdentificadorJugador(false, 1 + (MiId mod 2)) :- soyYoAIdentificadorJugador(true, MiId).

// Predicado que es verdadero si y solo si no es el primer turno del juego
// (es decir, alguien ha colocado una ficha)
noEsPrimerTurno :- tablero(_, _, Id) & Id \== 0.

// Inicializa el estado anterior del tablero, que se corresponde con el estado inicial.
// El tablero cambia de estado una sola vez cuando termina el turno de un jugador
inicializarTableroAnterior :- inicializarTableroAnterior_impl(0, 0).
inicializarTableroAnterior_impl(X, Y) :-
	X < 8 & Y < 8 &
	tablero(X, Y, Id) &
	.assertz(tableroAnterior(X, Y, Id)) &
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
	noEsPrimerTurno &
	movimientoRealizado(X, Y, Id) &
	tableroAnterior(X, Y, AnteriorId) &
	actualizarEstadoAnterior(X, Y, Id) &
	colocarFichaZobrist(X, Y, AnteriorId, Id).
// Si es el primer turno del juego, no hay nada que percibir
percibirMovimientoRival :- not noEsPrimerTurno.

// Inicializa la estrategia usada a la primera estrategia de la primera ronda
inicializarEstrategiaAnterior :-
	estrategia(Est) &
	.asserta(estrategiaAnterior(Est)).

// Predicados que vacían la tabla de transposiciones si ha ocurrido un cambio
// de estrategia o empieza una nueva partida, pues las transposiciones dejarían de tener validez
vaciarTablaTransposicionesSiAplicable :-
	estrategiaAnterior(EstAnt) &
	estrategia(Est) &
	EstAnt = Est &
	noEsPrimerTurno.
vaciarTablaTransposicionesSiAplicable :-
	((estrategiaAnterior(EstAnt) &
	estrategia(Est) &
	EstAnt \== Est &
	.abolish(estrategiaAnterior(_)) &
	.asserta(estrategiaAnterior(Est))) |
	not noEsPrimerTurno) &
	vaciarMapaZobrist.

heuristica(_, V) :- .random(V).

// Cláusula interfaz para comprobar si alguien ha ganado la partida o no. El segundo argumento existe para que se pueda guardar
// en caché el valor de salida de tal argumento, en vez de si se ha encontrado una solución o no.
// Diferencia principal entre esta regla y caracteristicaRaya: detiene la evaluación de rayas al encontrar la primera de 4,
// y no sigue hasta el final del tablero, por lo que es algo más eficiente
jugadorGano(_, false).// :- jugadorGano_impl(0, 0, Yo, ValorVerdad).

// Idea intuitiva: guardar las últimas jugadas procesadas por minimax, para que si se vuelven a consultar
// baste con consultar una tabla en lugar de tener que calcular minimax de nuevo

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

// El número de entradas actuales en la tabla de transposiciones
entradasMapaZobrist(0).

// Crea una entrada en la tabla de transposiciones para el estado actual.
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas) &
	Entradas < 16384 & // A 1 KiB por entrada (estimación a ojo de buen cubero pesimista), las entradas ocuparían 16 MiB
	eliminarDeMapaSiEsta &
	hashZobristActual(Hash) &
	.abolish(entradasMapaZobrist(_)) &
	.asserta(entradasMapaZobrist(Entradas + 1)) &
	.assertz(mapaZobrist(Hash, E)).
// Si llegamos al máximo de entradas, vaciar el mapa y volver a empezar
asociarEntradaEstadoActual(E) :-
	entradasMapaZobrist(Entradas) &
	Entradas >= 16384 &
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
	.asserta(entradasMapaZobrist(1)).

/* Objetivos iniciales */

!inicializarEstructurasDatos.

/* Planes */

// Inicializa las estructuras de datos del agente
+!inicializarEstructurasDatos[source(self)] : inicializarTableroAnterior & inicializarEstrategiaAnterior & inicializarZobrist.

// Analiza y realiza la mejor jugada decidible para el estado actual del tablero
+!hacerMejorJugada[source(self)] :
	estrategia(Est) & esei.si.alejandrogg.segundosJugadas(TiempoMax) &
	vaciarTablaTransposicionesSiAplicable & percibirMovimientoRival
<-
	+inicioAnalisis(system.time);
	?inicioAnalisis(Inicio);
	+profundidadBusqueda(1);

	// Realizar iterative deepening hasta que se agote el tiempo
	while (system.time - Inicio < TiempoMax * 1000) {
		?profundidadBusqueda(P);

		.print("Búsqueda de profundidad ", P, " en curso... (", system.time - Inicio, " ms transcurridos)");
		if (Est = jugarAGanar) {
			?mejorSiguienteColumna;
		} else {
			?peorSiguienteColumna;
		};

		-+profundidadBusqueda(P + 1);
	};

	// Mostrar cuánto hemos tardado
	.print("Movimiento analizado en ", system.time - Inicio, " ms");

	-inicioAnalisis(_);
	-profundidadBusqueda(_);
	-movimientoOptimo(X);

	// Realizar el movimiento a la columna calculada
	put(X).

// Reaccionar al evento de turno recibido
+turno(Yo)[source(percept)] : .my_name(Yo) <-
	.print("Turno recibido. Esperando por inicialización de estructuras de datos...");

	// Esperar a que se inicialicen todas las variables, si es necesario.
	// Esta espera devuelve el control inmediatamente si las creencias están en la BC
	.wait(tableroAnterior(_, _, _));
	.wait(estrategiaAnterior(_));
	.wait(hashZobristActual(_));

	.wait(750); // Por si estamos recibiendo todavía percepciones del tablero

	// Por si quedó otro predicado turno/1 en la BC.
	// Este .abolish pareció arreglar un extraño bug que no he podido reproducir
	// en mi ordenador, pero sí en el de Juan. Quizás se deba a alguna condición
	// de carrera. El bug hacía que el agente dejase de jugar tras el primer
	// movimiento
	.abolish(turno(_)[source(percept)]);

	.print("Estructuras de datos inicializadas. Analizando jugadas...");

	!hacerMejorJugada.

// Descartar comunicaciones que lleguen de otros agentes, pues solo nos interesa
// lo que diga el entorno
+!kqml_received(Agente, _, _, _) : .my_name(Yo) & Agente \== Yo.
