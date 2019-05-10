% Idea intuitiva: ir generando un árbol de juego de profundidad limitada, y
% escoger en cada nivel la jugada que maximiza el beneficio propio (nos lleva
% más cerca a la victoria) y minimiza el beneficio ajeno (hace más difícil que
% el rival gane)

% Elemento del árbol de juego (Jugada): [ movimiento(X, Y, SoyYoQuienHaceMov), movimiento(X, Y, SoyYoQuienHaceMov), ... ]

% Los segundos que debe de tardar en realizarse una jugada, a modo orientativo.
% La búsqueda puede tardar más de este tiempo, si se da la casualidad de que poco antes
% de que expire el tiempo incrementa el nivel de profundidad de la búsqueda
segundosJugadas(5).

% El valor devuelto por la heurística para señalar una victoria. Es el valor
% máximo posible que puede tomar la heurística
heuristicaVictoria(999999).
% El valor devuelto por la heurística para señalar una derrota. Es el valor
% mínimo posible que puede tomar la heurística
heuristicaDerrota(-999999).

estrategia(jugarAPerder).

% El estado actual del tablero. Se asume que el estado es completo
% y consistente
:- dynamic tablero/3.
tablero(0, 0, 0).
tablero(0, 1, 0).
tablero(0, 2, 0).
tablero(0, 3, 0).
tablero(0, 4, 0).
tablero(0, 5, 0).
tablero(0, 6, 0).
tablero(0, 7, 0).
tablero(1, 0, 0).
tablero(1, 1, 0).
tablero(1, 2, 0).
tablero(1, 3, 0).
tablero(1, 4, 0).
tablero(1, 5, 0).
tablero(1, 6, 0).
tablero(1, 7, 0).
tablero(2, 0, 0).
tablero(2, 1, 0).
tablero(2, 2, 0).
tablero(2, 3, 0).
tablero(2, 4, 0).
tablero(2, 5, 0).
tablero(2, 6, 0).
tablero(2, 7, 0).
tablero(3, 0, 0).
tablero(3, 1, 0).
tablero(3, 2, 0).
tablero(3, 3, 0).
tablero(3, 4, 0).
tablero(3, 5, 0).
tablero(3, 6, 0).
tablero(3, 7, 0).
tablero(4, 0, 0).
tablero(4, 1, 0).
tablero(4, 2, 0).
tablero(4, 3, 0).
tablero(4, 4, 0).
tablero(4, 5, 0).
tablero(4, 6, 0).
tablero(4, 7, 0).
tablero(5, 0, 0).
tablero(5, 1, 0).
tablero(5, 2, 0).
tablero(5, 3, 0).
tablero(5, 4, 0).
tablero(5, 5, 0).
tablero(5, 6, 0).
tablero(5, 7, 0).
tablero(6, 0, 0).
tablero(6, 1, 0).
tablero(6, 2, 0).
tablero(6, 3, 0).
tablero(6, 4, 0).
tablero(6, 5, 0).
tablero(6, 6, 0).
tablero(6, 7, 0).
tablero(7, 0, 0).
tablero(7, 1, 0).
tablero(7, 2, 0).
tablero(7, 3, 0).
tablero(7, 4, 0).
tablero(7, 5, 0).
tablero(7, 6, 0).
tablero(7, 7, 0).

:- consult("Zobrist.pl").

% Cláusulas interfaz para obtener la columna donde colocar una ficha para maximizar
% nuestra victoria o la del contrincante
mejorSiguienteColumna(X) :-
	vaciarTablaTransposicionesSiCambioEstrategia,
	percibirMovimientoRival,
	get_time(Ahora),
	mejorSiguienteColumna_impl(X, X, Ahora, 1).
peorSiguienteColumna(X) :-
	vaciarTablaTransposicionesSiCambioEstrategia,
	percibirMovimientoRival,
	get_time(Ahora),
	peorSiguienteColumna_impl(X, X, Ahora, 1).

% Llama a minimax con niveles crecientes de profundidad, hasta que se agote el tiempo máximo
% de selección de jugada. Es decir, implementa una búsqueda en profundidad iterativa
mejorSiguienteColumna_impl(X, _, TiempoInicio, ProfundidadActual) :-
	writeln("Iteración de búsqueda en curso..."),
	minimax([[movimiento(XNuevo, _, _)|_], _], ProfundidadActual),
	get_time(Ahora),
	Transcurrido is Ahora - TiempoInicio,
	siguienteIteracionMejor(X, XNuevo, TiempoInicio, Transcurrido, ProfundidadActual).

% Predicados que deciden si realizar una nueva iteración de búsqueda o no, dependiendo del tiempo que haya pasado
siguienteIteracionMejor(X, XActual, TiempoInicio, Transcurrido, ProfundidadActual) :-
	segundosJugadas(TiempoMax),
	Transcurrido < TiempoMax, % Si han pasado menos de TiempoMax segundos, realizar una iteración más
	ProfundidadNueva is ProfundidadActual + 1,
	mejorSiguienteColumna_impl(X, XActual, TiempoInicio, ProfundidadNueva).
siguienteIteracionMejor(X, X, _, Transcurrido, _) :-
	segundosJugadas(TiempoMax),
	Transcurrido >= TiempoMax. % Si han pasado TiempoMax segundos o más, interrumpir la búsqueda iterativa y quedarnos con la jugada actual

% Llama a maximin con niveles crecientes de profundidad, hasta que se agote el tiempo máximo
% de selección de jugada. Es decir, implementa una búsqueda en profundidad iterativa
peorSiguienteColumna_impl(X, _, TiempoInicio, ProfundidadActual) :-
	writeln("Iteración de búsqueda en curso..."),
	maximin([[movimiento(XNuevo, _, _)|_], _], ProfundidadActual),
	get_time(Ahora),
	Transcurrido is Ahora - TiempoInicio,
	siguienteIteracionPeor(X, XNuevo, TiempoInicio, Transcurrido, ProfundidadActual).

% Predicados que deciden si realizar una nueva iteración de búsqueda o no, dependiendo del tiempo que haya pasado
siguienteIteracionPeor(X, XActual, TiempoInicio, Transcurrido, ProfundidadActual) :-
	segundosJugadas(TiempoMax),
	Transcurrido < TiempoMax, % Si han pasado menos de TiempoMax segundos, realizar una iteración más
	ProfundidadNueva is ProfundidadActual + 1,
	peorSiguienteColumna_impl(X, XActual, TiempoInicio, ProfundidadNueva).
siguienteIteracionPeor(X, X, _, Transcurrido, _) :-
	segundosJugadas(TiempoMax),
	Transcurrido >= TiempoMax. % Si han pasado TiempoMax segundos o más, interrumpir la búsqueda iterativa y quedarnos con la jugada actual

% Cláusula interfaz para obtener la jugada óptima a realizar, con su heurística asociada
minimax(JugadaYHeuristica, P) :-
	heuristicaDerrota(MinimoMax), % MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo
	heuristicaVictoria(MaximoMin), % MaximoMin = Beta
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, false),
	% Borrar datos en caché y simulaciones temporales
	deshacerJugadaHecha.
:- dynamic haciendoMaximin/0.
% Cláusula interfaz para obtener la jugada óptima a realizar para perder en el juego, con su heurística asociada
maximin(JugadaYHeuristica, P) :-
	heuristicaDerrota(MinimoMax), % MinimoMax = Alfa. Encuentro el primer nombre de variable más intuitivo que una letra griega
	heuristicaVictoria(MaximoMin), % MaximoMin = Beta
	asserta(haciendoMaximin), % Para que no se tenga en cuenta la inversión del jugador que maximiza para generar jugadas
	minimax_impl([], JugadaYHeuristica, P, MinimoMax, MaximoMin, true),
	retract(haciendoMaximin),
	% Borrar datos en caché y simulaciones temporales
	deshacerJugadaHecha.
% Si la profundidad restante es cero, no generar hijos para este nodo,
% y considerar la heurística del nodo como la heurística de la jugada que representa
% (caso base)
minimax_impl(JugadaActual, [JugadaActual, Heuristica], 0, _, _, _) :-
	simularJugada(JugadaActual),
	heuristica(JugadaActual, Heuristica).
% Si la profundidad restante no es cero, generar hijos para este nodo del árbol
% y considerar la heurística del nodo como la heurística máxima o mínima de las jugadas
% hijas
minimax_impl(JugadaActual, [JugadaOptima, Heuristica], Profundidad, MinimoMax, MaximoMin, Maximizar) :-
	Profundidad > 0, % Para hacer esta regla exclusiva respecto de la primera
	consultarTablaTransposicion(JugadaActual, Profundidad, MinimoMax, MaximoMin, NuevoMinimoMax, NuevoMaximoMin, JugadaOptimaLeida, HeuristicaLeida),
	unificarCon(NuevoMinimoMax, MinimoMax),
	unificarCon(NuevoMaximoMin, MaximoMin),
	unificarCon(JugadaOptima, JugadaOptimaLeida),
	unificarCon(Heuristica, HeuristicaLeida),
	negar(Maximizar, NuevoMaximizar), % Para alternar entre max y min en cada nivel de profundidad del árbol
	seguirMinimaxSiNecesario(JugadaActual, [JugadaOptima, Heuristica], Profundidad, NuevoMinimoMax, NuevoMaximoMin, NuevoMaximizar).

% Sigue ejecutando minimax si y solo si lo leído de la tabla de transposiciones lo aconseja
seguirMinimaxSiNecesario(JugadaActual, [JugadaOptima, Heuristica], Profundidad, MinimoMax, MaximoMin, Maximizar) :-
	not(ground(JugadaOptima)), not(ground(Heuristica)), % .ground en Jason
	misJugadasTeniendoCuentaMaximin(Maximizar, MisJugadas),
	generarJugadasInmediatas(JugadaActual, Jugadas, MisJugadas),
	minimaxNodoArbol(JugadaActual, Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar).
seguirMinimaxSiNecesario(_, [JugadaOptima, Heuristica], _, _, _, _) :-
	ground(JugadaOptima), ground(Heuristica). % Hemos leído los datos necesarios de la tabla, así que no hacer minimax

% Consulta la tabla de transposición, obteniendo de ella cotas inferiores y superiores,
% así como la jugada más óptima en respuesta a la actual, si se ha determinado
consultarTablaTransposicion(Jugada, Profundidad, MinimoMax, MaximoMin, NuevoMinimoMax, NuevoMaximoMin, JugadaOptima, Heuristica) :-
	simularJugada(Jugada),
	enMapaZobrist,
	obtenerEntradaZobrist([ JugadaOptimaE, HeuristicaE, Tipo, ProfundidadE ]),
	ProfundidadComp is Profundidad - 1, % La tabla se rellena tras la resta
	(ProfundidadE < ProfundidadComp; % No se evalúa el segundo operando si el primero es cierto, pero añade un punto de regreso para obtener más soluciones
	procesarEntradaTablaTrans(Tipo, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin, JugadaOptima, Heuristica, NuevoMinimoMax, NuevoMaximoMin)).
consultarTablaTransposicion(Jugada, _, MinimoMax, MaximoMin, MinimoMax, MaximoMin, _, _) :-
	simularJugada(Jugada),
	not(enMapaZobrist).

% Predicados que leen de la entrada de la tabla de transposiciones las variables correspondientes
procesarEntradaTablaTrans(exacto, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin, JugadaOptimaE, HeuristicaE, MinimoMax, MaximoMin).
procesarEntradaTablaTrans(cotaInferior, _, Valor, MinimoMax, MaximoMin, _, _, NuevoMinimoMax, MaximoMin) :-
	valorMaximo(MinimoMax, Valor, NuevoMinimoMax).
procesarEntradaTablaTrans(cotaSuperior, _, Valor, MinimoMax, MaximoMin, _, _, MinimoMax, NuevoMaximoMin) :-
	valorMinimo(MaximoMin, Valor, NuevoMaximoMin).

% Los nodos que dan lugar a por lo menos una jugada posible deben de ser analizados
minimaxNodoArbol(JugadaActual, Jugadas, JugadaOptima, Heuristica, ProfundidadAnterior, MinimoMax, MaximoMin, Maximizar) :-
	Jugadas \= [],
	NuevaProfundidad is ProfundidadAnterior - 1,
	ordenarJugadas(JugadaActual, Jugadas, JugadasOrdenadas, NuevaProfundidad),
	minimaxVariasJugadas(JugadaActual, JugadasOrdenadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar).
% Los nodos sin más jugadas posibles son terminales, y su heurística se calcula directamente
minimaxNodoArbol(JugadaActual, [], JugadaActual, Heuristica, _, _, _, _) :-
	simularJugada(JugadaActual),
	heuristica(JugadaActual, Heuristica).

% Coloca como primera jugada aquella que hemos determinado que es el mejor movimiento actual
ordenarJugadas(JugadaActual, Jugadas, JugadasOrdenadas, _) :-
	simularJugada(JugadaActual),
	enMapaZobrist,
	obtenerEntradaZobrist([ JugadaOptima, _, exacto, _ ]),
	eliminarElementoLista(Jugadas, JugadaOptima, JugadasProc),
	JugadasOrdenadas = [JugadaOptima|JugadasProc].
ordenarJugadas(_, Jugadas, Jugadas, _).

% Cláusula interfaz para obtener el resultado de aplicar minimax con poda alfa-beta sobre
% un nivel del árbol de juego
minimaxVariasJugadas(JugadaActual, Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar) :-
	heuristicaVictoria(MaximaHeuristica),
	MaximaHeuristicaProc is MaximaHeuristica + 1,
	heuristicaDerrota(MinimaHeuristica),
	MinimaHeuristicaProc is MinimaHeuristica - 1,
	negar(Maximizar, MaximizarProc),
	valorOptimo(MinimaHeuristicaProc, MaximaHeuristicaProc, MaximizarProc, HeuristicaComp),
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, NuevaProfundidad, MinimoMax, MaximoMin, Maximizar, [], HeuristicaComp),
	actualizarTablaTransposiciones(JugadaActual, JugadaOptima, Heuristica, MinimoMax, MaximoMin, NuevaProfundidad).
% Si no quedan jugadas a las que aplicar minimax, no tenemos nada más que hacer (caso base)
minimaxVariasJugadas_impl([], JugadaOptimaActual, HeuristicaActual, _, _, _, _, JugadaOptimaActual, HeuristicaActual).
% Mientras queden jugadas a las que aplicar minimax, aplicar minimax con poda alfa-beta
minimaxVariasJugadas_impl([Jugada|Cdr], JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	minimax_impl(Jugada, [_, HeuristicaComp], Profundidad, MinimoMax, MaximoMin, Maximizar),
	valorOptimo(HeuristicaActual, HeuristicaComp, Maximizar, HeuristicaOpt),
	jugadaOptima(HeuristicaActual, HeuristicaOpt, JugadaOptimaActual, Jugada, JugadaOpt),
	valorOptimoCondicional(Maximizar, MinimoMax, HeuristicaOpt, true, NuevoMinimoMax),
	negar(Maximizar, MaximizarNegado),
	valorOptimoCondicional(MaximizarNegado, MaximoMin, HeuristicaOpt, false, NuevoMaximoMin),
	podaAlfaBeta(NuevoMinimoMax, NuevoMaximoMin, Cdr, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOpt, HeuristicaOpt).

% Realiza o no la poda alfa-beta en el nivel actual del árbol de juego, dependiendo de si se cumple
% la condición de poda (MinimoMax mayor o igual que MaximoMin) o no
podaAlfaBeta(MinimoMax, MaximoMin, Jugadas, JugadaOptima, Heuristica, Profundidad, Maximizar, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax < MaximoMin, % No se cumple condición de poda
	minimaxVariasJugadas_impl(Jugadas, JugadaOptima, Heuristica, Profundidad, MinimoMax, MaximoMin, Maximizar, JugadaOptimaActual, HeuristicaActual).
podaAlfaBeta(MinimoMax, MaximoMin, _, JugadaOptimaActual, HeuristicaActual, _, _, JugadaOptimaActual, HeuristicaActual) :-
	MinimoMax >= MaximoMin. % Se cumple condición de poda. No seguir analizando jugadas posibles por aquí

% Actualiza la tabla de transposiciones con los valores deseables
actualizarTablaTransposiciones(JugadaActual, JugadaOptima, Heuristica, MinimoMax, MaximoMin, Profundidad) :-
	deducirTipoEntrada(Heuristica, MinimoMax, MaximoMin, Tipo),
	simularJugada(JugadaActual),
	asociarEntradaEstadoActual([ JugadaOptima, Heuristica, Tipo, Profundidad ]).

% Obtiene el tipo de entrada a guardar en la tabla de transposiciones, para actualizarTablaTransposiciones/6
deducirTipoEntrada(Heuristica, MinimoMax, _, cotaSuperior) :-
	MinimoMax >= Heuristica.
deducirTipoEntrada(Heuristica, _, MaximoMin, cotaInferior) :-
	Heuristica >= MaximoMin.
deducirTipoEntrada(Heuristica, MinimoMax, MaximoMin, exacto) :-
	Heuristica < MaximoMin, Heuristica > MinimoMax.

% Si las heurísticas a comparar difieren, entonces la última jugada que se pasa como parámetro se
% unifica con el último parámetro (nueva jugada óptima).
% En caso contrario, si son iguales, entonces el último parámetro se unifica con la primera jugada
% que se pasa como parámetro (la jugada óptima se queda como está).
jugadaOptima(HeuristicaActual, HeuristicaOpt, _, Jugada, Jugada) :-
	HeuristicaActual \= HeuristicaOpt.
jugadaOptima(HeuristicaActual, HeuristicaOpt, JugadaOptimaActual, _, JugadaOptimaActual) :-
	HeuristicaActual = HeuristicaOpt.

% Devuelve el valor apropiado de MisJugadas para la generación de jugadas, teniendo en cuenta si se está ejecutando minimax o bien maximin.
% Esencialmente, estas cláusulas hacen que resultado = MiJugada XOR haciendoMaximin.
misJugadasTeniendoCuentaMaximin(MisJugadas, true) :- (not(MisJugadas), haciendoMaximin); (MisJugadas, not(haciendoMaximin)).
misJugadasTeniendoCuentaMaximin(MisJugadas, false) :- (not(MisJugadas), not(haciendoMaximin)); (MisJugadas, haciendoMaximin).

% Cláusulas interfaz para generar las jugadas inmediatas a partir de una jugada que
% se considera ya hecha (aunque realmente no sea así)
% Si nadie gana, y el tablero no está lleno, entonces generar jugadas
generarJugadasInmediatas(JugadaHecha, JugadasGeneradas, MisJugadas) :-
	simularJugada(JugadaHecha),
	jugadorGano(true, VictoriaMia),
	jugadorGano(false, VictoriaRival),
	not(VictoriaMia), not(VictoriaRival), not(tableroLleno),
	generarJugadasInmediatas_impl(0, JugadaHecha, difListas(JugadasGeneradas, []), MisJugadas).
% Si alguien ha ganado, o el tablero está lleno, no hay más jugadas posibles
generarJugadasInmediatas(JugadaHecha, [], _) :-
	simularJugada(JugadaHecha),
	jugadorGano(true, VictoriaMia),
	jugadorGano(false, VictoriaRival),
	(VictoriaMia; VictoriaRival; tableroLleno).
% Si hay una columna siguiente, y tiene hueco para una ficha, entonces generar una nueva jugada con ella,
% y añadirla a la lista
generarJugadasInmediatas_impl(X, JugadaHecha, JugadasGeneradas, MisJugadas) :-
	X >= 0, X < 8,
	tablero(X, 0, 0),
	SigX is X + 1,
	generarJugadasInmediatas_impl(SigX, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas),
	calcularGravedad(X, Y),
	append_simple(JugadaHecha, [movimiento(X, Y, MisJugadas)], NuevaJugada), % No vamos a tener que iterar sobre muchas jugadas
	append_dl(difListas([NuevaJugada|Cdr], Cdr), difListas(InicioJugadasGeneradas, FinJugadasGeneradas), JugadasGeneradas).
% Si hay una columna siguiente, pero no hay hueco para una ficha, continuar iteraciones sin añadir nuevas jugadas
generarJugadasInmediatas_impl(X, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas) :-
	X >= 0, X < 8,
	tablero(X, 0, Jugador),
	Jugador \= 0,
	SigX is X + 1,
	generarJugadasInmediatas_impl(SigX, JugadaHecha, difListas(InicioJugadasGeneradas, FinJugadasGeneradas), MisJugadas).
% Si no hay una columna siguiente, las nuevas jugadas generadas se corresponden
% con la lista vacía (caso base)
generarJugadasInmediatas_impl(X, _, difListas(JugadasGeneradas, JugadasGeneradas), _) :-
	X < 0; X >= 8.

% Comprueba si el tablero está lleno
tableroLleno :- not(tablero(_, _, 0)).

% Sin jugada que aplicar, no hacer nada (caso base)
aplicarJugada([]).
% Aplicar cada uno de los movimientos
aplicarJugada([Movimiento|Cdr]) :-
	aplicarMovimiento(Movimiento),
	aplicarJugada(Cdr).

% Simula un movimiento en el tablero
aplicarMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id),
	retract(tablero(X, Y, 0)), % abolish en Jason
	asserta(tablero(X, Y, Id)),
	colocarFichaZobrist(X, Y, 0, Id).
	% TODO: crear y mantener predicados raya, que indiquen la posición de las rayas formadas, por quién y de qué longitud

:- dynamic jugadaHecha/1.
% Aplica una jugada, deshaciendo cualquier otra jugada simulada anterior, para evitar incongruencias.
% En caso de que ya esté aplicada, devuelve verdadero igualmente
simularJugada(Jugada) :-
	not(jugadaHecha(Jugada)),
	deshacerJugadaHecha,
	aplicarJugada(Jugada),
	asserta(jugadaHecha(Jugada)).
simularJugada(Jugada) :- jugadaHecha(Jugada).

% Sin jugada que deshacer, no hacer nada (caso base)
deshacerJugada([]).
% Deshacer cada uno de los movimientos
deshacerJugada([Movimiento|Cdr]) :-
	deshacerMovimiento(Movimiento),
	deshacerJugada(Cdr).

% Deshace la simulación de un movimiento en el tablero
deshacerMovimiento(movimiento(X, Y, SoyYoQuienHaceMov)) :-
	soyYoAIdentificadorJugador(SoyYoQuienHaceMov, Id),
	retract(tablero(X, Y, Id)), % abolish en Jason
	asserta(tablero(X, Y, 0)),
	colocarFichaZobrist(X, Y, Id, 0).

% Deshace la jugada hecha actualmente. Si no se ha hecho una, devuelve verdadero igualmente
deshacerJugadaHecha :-
	jugadaHecha(Jugada),
	deshacerJugada(Jugada),
	retract(jugadaHecha(Jugada)).
deshacerJugadaHecha :- not(jugadaHecha(_)).

% Calcula la coordenada vertical donde caería una ficha colocada en la columna X
calcularGravedad(X, Y) :-
	calcularGravedad_impl(X, Y, 7).
% Si hay un hueco disponible en la Y actual, es ahí donde cae (caso base)
calcularGravedad_impl(X, YActual, YActual) :-
	YActual >= 0, YActual < 8,
	tablero(X, YActual, 0).
% Si no hay un hueco en la ordenada actual, pero todavía estamos en rango, seguir comprobando
calcularGravedad_impl(X, Y, YActual) :-
	YActual >= 0, YActual < 8,
	tablero(X, YActual, Jugador),
	Jugador \= 0,
	YSig is YActual - 1,
	calcularGravedad_impl(X, Y, YSig).

% Concatena dos listas expresadas como diferencias de listas.
% Esta operación es de complejidad O(1)
append_dl(difListas(Inicio1, Fin1), difListas(Fin1, Fin2), difListas(Inicio1, Fin2)).

% Concatena dos listas de manera trivial.
% Esta operación es de complejidad O(n), pero funciona en listas cerradas
append_simple([], L, L).
append_simple([Car|Cdr], L, [Car|R]) :- append_simple(Cdr, L, R).

% Obtiene el valor mínimo de dos variables instanciadas
valorMinimo(A, B, A) :- A < B.
valorMinimo(A, B, B) :- A >= B.

% Obtiene el valor máximo de dos variables instanciadas
valorMaximo(A, B, B) :- B >= A.
valorMaximo(A, B, A) :- A > B.

% Obtiene el valor óptimo de entre los dados, que puede ser el máximo o el mínimo, dependiendo de un argumento que se pase
valorOptimo(A, B, true, Maximo) :- valorMaximo(A, B, Maximo).
valorOptimo(A, B, false, Minimo) :- valorMinimo(A, B, Minimo).

% Si el primer parámetro es verdadero, Resultado se unificará con el valor óptimo entre los dos dados.
% En el caso contrario, si el primer parámetro es falso, se unificará con el valor del segundo parámetro.
valorOptimoCondicional(true, A, B, Maximizar, Resultado) :- valorOptimo(A, B, Maximizar, Resultado).
valorOptimoCondicional(false, A, _, _, A).

% Obtiene la negación de un valor de verdad en un argumento, utilizando la negación por fallo disponible en Prolog
negar(ValorVerdad, false) :- ValorVerdad.
negar(ValorVerdad, true) :- not(ValorVerdad).

% Predicados que unifican una variable libre con otra, que también puede ser libre.
% Si la variable no es libre, no hacen nada
unificarCon(VariableLibre, Valor) :-
	not(ground(VariableLibre)), % .ground en Jason
	VariableLibre = Valor.
unificarCon(VariableLibre, _) :-
	ground(VariableLibre).

% Predicados que eliminan las ocurrencias de un elemento en una lista cerrada
% La lista vacía no tiene nada que eliminar (caso base)
eliminarElementoLista([], _, []).
eliminarElementoLista([Car|Cdr], E, [Car|R]) :-
	Car \= E, % Un elemento diferente de aquel a eliminar se incluye en el resultado
	eliminarElementoLista(Cdr, E, R).
eliminarElementoLista([Car|Cdr], E, R) :-
	Car = E, % El elemento a eliminar no se incluye en el resultado
	eliminarElementoLista(Cdr, E, R).

soyYoAIdentificadorJugador(true, 1).
soyYoAIdentificadorJugador(false, 2).

% Predicado que es verdadero si y solo si no es el primer turno del juego
% (es decir, alguien ha colocado una ficha)
noEsPrimerTurno :- tablero(_, _, Id), Id \= 0.

:- dynamic tableroAnterior/3.
% Inicializa el estado anterior del tablero, que se corresponde con el estado inicial.
% El tablero cambia de estado una sola vez cuando termina el turno de un jugador
inicializarTableroAnterior :- inicializarTableroAnterior_impl(0, 0).
inicializarTableroAnterior_impl(X, Y) :-
	X < 8, Y < 8,
	tablero(X, Y, Id),
	assertz(tableroAnterior(X, Y, Id)),
	XSig is X + 1,
	inicializarTableroAnterior_impl(XSig, Y).
% Si hemos terminado de procesar la fila actual, ir con la siguiente
inicializarTableroAnterior_impl(X, Y) :-
	X = 8, Y < 7,
	YSig is Y + 1,
	inicializarTableroAnterior_impl(0, YSig).
% Si hemos terminado de procesar todo el tablero, no hay más trabajo que hacer aquí
inicializarTableroAnterior_impl(X, Y) :-
	X = 8, Y = 7.

% Obtiene el movimiento que se ha realizado desde el estado anterior del tablero
movimientoRealizado(X, Y, IdNuevo) :-
	tableroAnterior(X, Y, IdPrevio),
	tablero(X, Y, IdNuevo),
	IdPrevio \= IdNuevo.

% Actualiza el estado anterior del tablero con un nuevo movimiento
actualizarEstadoAnterior(X, Y, Id) :-
	retract(tableroAnterior(X, Y, _)),
	assertz(tableroAnterior(X, Y, Id)).

% Si no es el primer turno del juego, actualizar tabla de transposiciones e información del estado anterior
percibirMovimientoRival :-
	noEsPrimerTurno,
	movimientoRealizado(X, Y, Id),
	tableroAnterior(X, Y, AnteriorId),
	actualizarEstadoAnterior(X, Y, Id),
	colocarFichaZobrist(X, Y, AnteriorId, Id).
% Si es el primer turno del juego, no hay nada que percibir
percibirMovimientoRival :- not(noEsPrimerTurno).

:- dynamic estrategiaAnterior/1.
% Inicializa la estrategia usada a la primera estrategia de la primera ronda
inicializarEstrategiaAnterior :-
	estrategia(Est),
	asserta(estrategiaAnterior(Est)).

% Predicados que vacían la tabla de transposiciones si ha ocurrido un cambio
% de estrategia, pues las transposiciones dejarían de tener validez
vaciarTablaTransposicionesSiCambioEstrategia :-
	estrategiaAnterior(EstAnt),
	estrategia(Est),
	EstAnt = Est.
vaciarTablaTransposicionesSiCambioEstrategia :-
	estrategiaAnterior(EstAnt),
	estrategia(Est),
	EstAnt \= Est,
	retract(estrategiaAnterior(_)),
	asserta(estrategiaAnterior(Est)),
	vaciarMapaZobrist.

% Realizar tareas de inicialización
:- inicializarTableroAnterior.
:- inicializarEstrategiaAnterior.