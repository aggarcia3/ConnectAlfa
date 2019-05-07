import jason.asSyntax.*;
import jason.environment.Environment;
import jason.environment.grid.Location;
import javax.swing.JFrame;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Random;
import java.util.logging.Logger;
import java.awt.Canvas;

import java.awt.FontMetrics;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

import javax.swing.ImageIcon;


public class Blackboard extends Environment implements Runnable {

    public static final int GSize = 8; // grid size
    public static final int PIEZA_P1  = 16; // P1 Piece code in grid model
	public static final int PIEZA_P2  = 17; // P2 Piece code in grid model

    static Logger logger = Logger.getLogger(Blackboard.class.getName());

    private BlackboardModel model;
    private BlackboardView  view;
    
	// Turnos posibles
	private int turno = 0;
	private final int TURNO_NO_ESPECIFICADO = -1;
	private final int TURNO_PLAYER1 = 1;
	private final int TURNO_PLAYER2 = 2;
	
	// Estados de la partida
	private int estado;
	private final int JUGANDO = 0;
	private final int P1_RANGO_INCORRECTO = 1;
	private final int P2_RANGO_INCORRECTO = 2;
	private final int P1_COLUMNA_LLENA = 3;
	private final int P2_COLUMNA_LLENA = 4;
	private final int P1_GANADOR = 5;
	private final int P2_GANADOR = 6;
	private final int REINICIAR = 7;
	private final int CHECK_FIN_JUEGO = 8;
	private final int JUGAR_A_PERDER = 9;
	private final int EMPATE = 10;
	private final int FIN_JUEGO = 11;
	private final int P1_JUEGA_DOS_VECES = 12;
	private final int P2_JUEGA_DOS_VECES = 13;
	
	// Contador partidas
	private int p1_wins = 0;
	private int p2_wins = 0;
	private int empates = 0;
	private int numPartidas = 0;
	// Almacena quien es el que empezo el primero en la ultima partida
	private int turnoUltimaPartida = 1;
	
	// Modos de juego
	private int modoJuego = 0;
	private int MODO_JUGAR_A_GANAR = 1;
	private int MODO_JUGAR_A_PERDER = 2;
	
	// Ultima ficha
	private Ficha ultimaFicha;
	
	private int max_partidas = 0; 
	// Numero de partidas normal
	private final int MAX_PARTIDAS = 10;
	// Numero de partidas en caso de empate
	private final int MAX_PARTIDAS_RONDAS_EXTRA = 6;
	
	// Se utiliza para mantener un estado durante un tiempo controlado
	private Timer timer;
	
	// Control del numero de jugadas seguidas
	private int jugadasSeguidas;
	
    @Override
    public void init(String[] args) {
		// Nos va a indicar la posicion de la ultima ficha
		ultimaFicha = new Ficha();
		timer = new Timer();
		
        model = new BlackboardModel();
        view  = new BlackboardView(model);
		model.setView(view);
		
		Thread t = new Thread(this);
		t.start();
		
		// Parametros de inicio
		// Asignamos el primer turno
		turno = TURNO_PLAYER1;
		estado = JUGANDO;
		max_partidas = MAX_PARTIDAS;
		modoJuego = MODO_JUGAR_A_PERDER;
		view.setModoJuego(modoJuego);
		
		start();
    }
	
	/**
	 * Inicia una nueva partida
	 */
	private void start() {
		// Vaciamos el modelo
		clearPercepts();
		model.resetModel();
		view.resetView();
		estado = JUGANDO;
		view.setNuevaPartida();
		logger.info("turno: " + turno);
		numPartidas++;
		view.setRonda(numPartidas);

		updatePercepts();
	}
    
    @Override
    public boolean executeAction(String ag, Structure action) {
        logger.info(ag+" doing: "+ action + ", turno:" + turno);
		clearPercepts();
		
        try {
			if (estado == JUGANDO) {
					if (action.getFunctor().equals("put")) {
						int x = (int)((NumberTerm)action.getTerm(0)).solve();
						
						try {
							if (ag.equals("player1")) {
								// mover a personaje1
								model.put(PIEZA_P1,x);
							} else if (ag.equals("player2") ) {							
								model.put(PIEZA_P2,x);
							}
						} catch (Exception ex) {
						}
						
						// Compruebo si se quiere jugar dos veces
						if (ag.equals("player1") && turno != TURNO_PLAYER1) {
							estado = P1_JUEGA_DOS_VECES;
						} else if (ag.equals("player2") && turno != TURNO_PLAYER2) {
							estado = P2_JUEGA_DOS_VECES;
						} else {		
							// Cambio el turno
							turno = pasarTurno();
							// Comprobar reglas
							estado = checkTablero();
						}
				} else {
					return false;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
        updatePercepts();
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
		}
		informAgsEnvironmentChanged();
        return true;
    }
	
	public void run() {
		while(true) {
			// Posibles estados
			switch(estado) {
				case CHECK_FIN_JUEGO:
					// Comprobamos si esta es la ultima partida
					if (numPartidas == max_partidas) {
						int ganador = 0;
						if (p1_wins > p2_wins)
							ganador = P1_GANADOR;
						else if (p1_wins < p2_wins) {
							ganador = P2_GANADOR;
						} else {
							// Si han quedado en empate
							// vamos con dos rondas extras en que se juega
							// a ganar
							ganador = EMPATE;
							max_partidas = MAX_PARTIDAS_RONDAS_EXTRA;
							// Jugar a perder
							modoJuego = MODO_JUGAR_A_GANAR;
							view.setModoJuego(modoJuego);
						}
						// Si es la ultima partida finalizamos el juego
						if (numPartidas == max_partidas) {
							view.setFinJuego(ganador);
							estado = FIN_JUEGO;
						}
					} else {
						// Una espera antes de pasar a la siguiente partida
						if (timer.cronometro(5000.0)) {
							// Cambiamos el turno del que empieza
							turno = turnoUltimaPartida;
							turno = pasarTurno();
							turnoUltimaPartida = turno;
							view.setJugadorTurnoInicial(turno);
							// Comenzamos una nueva partida
							start();
						}
					}
					break;
				case P1_JUEGA_DOS_VECES:
					logger.info("Player1 juega dos veces");	
					view.setFinPartida(P2_GANADOR);
					anotar(P2_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P2_JUEGA_DOS_VECES:
					logger.info("Player2 juega dos veces");
					view.setFinPartida(P1_GANADOR);
					anotar(P1_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P1_RANGO_INCORRECTO:
					view.setFinPartida(P2_GANADOR);
					anotar(P2_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P1_COLUMNA_LLENA:
					view.setFinPartida(P2_GANADOR);
					anotar(P2_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P2_RANGO_INCORRECTO:
					view.setFinPartida(P1_GANADOR);
					anotar(P1_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P2_COLUMNA_LLENA:
					view.setFinPartida(P1_GANADOR);
					anotar(P1_GANADOR);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P1_GANADOR:
					if (modoJuego == MODO_JUGAR_A_GANAR) {
						logger.info("Ganador player1");	
						view.setFinPartida(P1_GANADOR);
						anotar(P1_GANADOR);
					} else if (modoJuego == MODO_JUGAR_A_PERDER) {
						logger.info("Ganador player2");
						view.setFinPartida(P2_GANADOR);
						anotar(P2_GANADOR);
					}
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case P2_GANADOR:
					if (modoJuego == MODO_JUGAR_A_GANAR) {
						logger.info("Ganador player2");
						view.setFinPartida(P2_GANADOR);
						anotar(P2_GANADOR);
					} else if (modoJuego == MODO_JUGAR_A_PERDER) {
						logger.info("Ganador player1");
						view.setFinPartida(P1_GANADOR);
						anotar(P1_GANADOR);
					}
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				case EMPATE:
					logger.info("Empate");
					view.setFinPartida(EMPATE);
					anotar(EMPATE);
					estado = CHECK_FIN_JUEGO;
					timer.start();
					break;
				default:
					break;
			}
			
			try {
				Thread.sleep(160);
			} catch (InterruptedException ex) {
			}                                               
			view.getDrawArea().repaint();
		}
	}
	
	/**
	 * Anota una victoria, derrota o empate
	 */
	 private void anotar(int tipo_estado) {
		 // Actualiza la puntuacion de los jugadores
		 if (modoJuego == MODO_JUGAR_A_PERDER) {
			 switch (tipo_estado) {
				  case P1_GANADOR:
						if (turnoUltimaPartida == TURNO_PLAYER1) {
							p1_wins+=2;
						} else {
							p1_wins+=3;	
						}
						break;
				  case P2_GANADOR: 
					  if (turnoUltimaPartida == TURNO_PLAYER2) {
						  p2_wins+=2; 
					  } else {
						  p2_wins+=3;
					  }
					  break;
				  case EMPATE:
						p1_wins++;
						p2_wins++; 
					  break; 
			 }
		 } else if (modoJuego == MODO_JUGAR_A_GANAR) {
			 switch (tipo_estado) {
				  case P1_GANADOR:
						if (turnoUltimaPartida == TURNO_PLAYER1) {
							p1_wins+=1;
						} else {
							p1_wins+=2;
						}
						break;
				  case P2_GANADOR: 
					  if (turnoUltimaPartida == TURNO_PLAYER2) {
						  p2_wins+=1;
					  } else {
						  p2_wins+=2;
					  }
					  break;
				  case EMPATE:
					  // Nadie puntua 
					  break; 
			 }
		 }
		 // Actualiza el marcador de la vista
		 view.setP1Score(p1_wins);
		 view.setP2Score(p2_wins);
		 view.setRonda(numPartidas);
	 }
	/**
	 * Comprueba el estado de la partida
	 */
	private int checkTablero() throws Exception {
		int estadoPartida = JUGANDO;
		
		// Comprobamos si el tablero esta lleno
		if (model.isAllBusy()) {
			estadoPartida = EMPATE;
		// Comprueba si intenta ponerse una ficha en una columna invalida
		// en cuyo caso perderia de forma instantanea
		} else if (ultimaFicha.getX() < 0 || ultimaFicha.getX() >= GSize) {
			if (ultimaFicha.getTipo() == PIEZA_P1) {
				logger.info("player1 ha puesto ficha fuera del tablero");
				estadoPartida = P1_RANGO_INCORRECTO;
			} else {
				logger.info("player2 ha puesto ficha fuera del tablero");
				estadoPartida = P2_RANGO_INCORRECTO;
			}
		} 
		// Comprueba si intenta poner una ficha en una columna que esta
		// llena, en cuyo caso perderia de forma instantanea
		else if (ultimaFicha.getY() == -1) {
			if (ultimaFicha.getTipo() == PIEZA_P1) {
				logger.info("player1 ha puesto ficha en una columna llena");
				estadoPartida = P1_COLUMNA_LLENA;
			} else {
				logger.info("player2 ha puesto ficha en una columna llena");
				estadoPartida = P2_COLUMNA_LLENA;
			}
		}
		// Comprueba si hay ganador
		else if (checkGanador(ultimaFicha.getTipo())) {
			if (ultimaFicha.getTipo() == PIEZA_P1) {
				estadoPartida = P1_GANADOR;
			} else {
				estadoPartida = P2_GANADOR;
			}
		}
		
		return estadoPartida;
	}
	/**
	 * Comprueba que haga 4 en raya en vertical
	 */
	private boolean checkGanadorVertical(int tipoJugador) throws Exception {
		int cont = 0;
		// Verticales
		for (int i = ultimaFicha.getY(); i < GSize && cont < 4; i++ ) {
			if (model.getData()[ultimaFicha.getX()][i] == tipoJugador) {
				view.addFichaGanadora(ultimaFicha.getX(), i);
				cont++;
			} else {
				view.resetFichasGanadoras();
				cont = 0;
			}
		}
		return (cont == 4);
	}
	/**
	 * Comprueba que haga 4 en raya en diagonal
	 */
	private boolean checkGanadorDiagonal(int tipoJugador) {
		int cont = 0;

		int fila = ultimaFicha.getY();
		int col = ultimaFicha.getX();
		
		//Diagonal que va desde el (0,0) hasta (7,7)
		int filaSuperiorDiagonalIzquierda = 0;
		int colSuperiorDiagonalIzquierda = 0;
		int filaInferiorDiagonalIzquierda = 0;
		int colInferiorDiagonalIzquierda = 0;
		
		//hacia arriba
		if (fila  <= col ){ //parte superior
			filaSuperiorDiagonalIzquierda = 0;
			colSuperiorDiagonalIzquierda = col -fila;
		}else{ //parte inferior
			filaSuperiorDiagonalIzquierda = fila - col;
			colSuperiorDiagonalIzquierda = 0;
		}
		
		//hacia abajo
		if (fila  <= col ){ //parte superior
			filaInferiorDiagonalIzquierda = fila + 7 - col;
			colInferiorDiagonalIzquierda = 7;
		}else{ //parte inferior
			filaInferiorDiagonalIzquierda = 7;
			colInferiorDiagonalIzquierda = 7 -fila + col;
		}
		
		int j = colSuperiorDiagonalIzquierda;
		for(int i = filaSuperiorDiagonalIzquierda; i <= filaInferiorDiagonalIzquierda; i++ ){
			if(model.getData()[j][i] == tipoJugador){
				view.addFichaGanadora(j, i);
				cont++;
				if(cont == 4) return true;
			}else{
				view.resetFichasGanadoras();
				cont = 0;
			}
			j++;
			
		}
		
		cont = 0;
		
		//Diagonal que va desde el (0,7) hasta (7,0)
		int filaSuperiorDiagonalDerecha = 0;
		int colSuperiorDiagonalDerecha = 0;
		int filaInferiorDiagonalDerecha = 0;
		int colInferiorDiagonalDerecha = 0;
		
		//hacia arriba
		if (fila + col  <= 7 ){ //parte superior
			filaSuperiorDiagonalDerecha = 0;
			colSuperiorDiagonalDerecha = fila + col;
		}else{ //parte inferior
			filaSuperiorDiagonalDerecha = col - 7 + fila;
			colSuperiorDiagonalDerecha = 7;
		}
		
		//hacia abajo
		if (fila + col  <= 7 ){ //parte superior
			filaInferiorDiagonalDerecha = fila + col;
			colInferiorDiagonalDerecha = 0;
		}else{ //parte inferior
			filaInferiorDiagonalDerecha = 7;
			colInferiorDiagonalDerecha = col -7 + fila;
		}
		
		j = colInferiorDiagonalDerecha;
		for(int i = filaInferiorDiagonalDerecha; i >= filaSuperiorDiagonalDerecha; i-- ){
			if(model.getData()[j][i] == tipoJugador){
				view.addFichaGanadora(j, i);
				cont++;
				if(cont == 4) return true;
			}else{
				view.resetFichasGanadoras();
				cont = 0;
			}
			j++;
			
		}
		
		cont = 0;
		return false;
	}
	/**
	 * Comprueba que haga 4 en raya con otras fichas en horizontal
	 */
	private boolean checkGanadorHorizontal(int tipoJugador) throws Exception {
		//horizontales
		int cont = 0;
		for(int col = 0; col < GSize && cont < 4; col++){
			if(model.getData()[col][ultimaFicha.getY()] == tipoJugador){
				view.addFichaGanadora(col, ultimaFicha.getY());
				cont++;
			} else {
				view.resetFichasGanadoras();
				cont =0;
			}
		}
		return (cont == 4);
	}
	/**
	 * Al introducir una ficha, comprueba que haya ganador
	 */
	private boolean checkGanador(int tipoJugador) throws Exception {
		return (checkGanadorHorizontal(tipoJugador)
				|| checkGanadorDiagonal(tipoJugador)
				|| checkGanadorVertical(tipoJugador));
	}
    
	private int pasarTurno() {
		//if (turno == TURNO_NO_ESPECIFICADO) return TURNO_NO_ESPECIFICADO;
		return (turno == TURNO_PLAYER1?TURNO_PLAYER2:TURNO_PLAYER1);
	}
	
    /** creates the agents perception based on the BlackboardModel */
    void updatePercepts() {
		// Sumamos el modelo del mundo
		StringBuffer sb = new StringBuffer("tablero([");
		int v = 0;
		for (int i = 0; i < GSize; i++) {
			for (int j = 0; j < GSize; j++) {
				v = 0;
				sb.append(model.getData()[j][i]);
				sb.append(",");
				if (model.getData()[j][i] > 0) {v = model.getData()[j][i]-15;};
				addPercept(Literal.parseLiteral("tablero("+j+","+i+","+v+")"));
			}
		}
		sb.deleteCharAt(sb.length()-1);
		sb.append("])");
		Literal l = Literal.parseLiteral(sb.toString());
		//addPercept(l);
		
		// Si estamos jugando y se ha asignado ya algun turno
		if (estado == JUGANDO && turno != TURNO_NO_ESPECIFICADO) {
			if (turno == TURNO_PLAYER1) {
				l = Literal.parseLiteral("turno(player1)");
			} else if (turno == TURNO_PLAYER2) {
				l = Literal.parseLiteral("turno(player2)");
			}
			addPercept(l);
		}
		// Cambio de estategia
		if (modoJuego == MODO_JUGAR_A_PERDER) {
			l = Literal.parseLiteral("estrategia(jugarAPerder)");
			addPercept(l);
		} else if (modoJuego == MODO_JUGAR_A_GANAR) {
			l = Literal.parseLiteral("estrategia(jugarAGanar)");
			addPercept(l);
		}
    }

	class Ficha {
		private int tipo, x, y;
		public void setPos(int tipo, int x, int y) {
			this.tipo = tipo;
			this.x = x;
			this.y = y;
		}
		public int getTipo() {
			return tipo;
		}
		public int getX() {
			return x;
		}
		public int getY() {
			return y;
		}
	}
	
    class BlackboardModel extends CustomModel {
        public static final int MErr = 2; // max error in pick garb
        int nerr; // number of tries of pick garb
        boolean r1HasGarb = false; // whether r1 is carrying garbage or not

        Random random = new Random(System.currentTimeMillis());

        private BlackboardModel() {
            super(GSize, GSize, 2);
        }
        
		/**
		 * Limpia el tablero para una nueva partida
		 */
		public void resetModel() {
			 for (int i = 0; i < GSize ; i++) {
				 for (int j = 0; j < GSize; j++) {
					 data[i][j] = 0;
				 }
			 }
		}
		
        void put(int tipoPieza, int x) throws Exception {
           int y = 0;
		   ultimaFicha.setPos(tipoPieza, x, -1);
		   // La coloca encima de otra
		   while ((data[x][y] != PIEZA_P1 && data[x][y] != PIEZA_P2) 
		   		  && y < 7) {
				y++;
		   }
		   if ((data[x][y] == PIEZA_P1 || data[x][y] == PIEZA_P2)) {
			   y = y-1;
		   }
		   ultimaFicha.setPos(tipoPieza, x, y);
		   add(tipoPieza, x, y);
		}
    }
	
    class BlackboardView extends CustomView {
		private int pos[][] = new int[GSize][GSize];
		// Almacena cuales de las fichas son las ganadoras
		private boolean fichasGanadoras[][] = new boolean[GSize][GSize];

        public BlackboardView(BlackboardModel model) {
            super(model, "Conecta4 - SI 2018/2019", 200);
            defaultFont = new Font("Arial", Font.BOLD, 18); // change default font
			
			timer = new Timer();
			
            setVisible(true);
            repaint();
        }
		
		public void addFichaGanadora(int x, int y) {
			fichasGanadoras[x][y] = true;
		}
		
		public boolean isFichaGanadora(int x, int y) {
			return fichasGanadoras[x][y];
		}
		
		public void resetFichasGanadoras() {
			fichasGanadoras = new boolean[GSize][GSize];
		}
		
		public void resetView() {
			for (int i = 0; i < GSize ; i++) {
				 for (int j = 0; j < GSize; j++) {
					 pos[i][j] = 0;
				 }
			 }
			 resetFichasGanadoras();
		}
		
        /** draw application objects */
        @Override
        public void draw(Graphics g, int x, int y, int object) {
			switch (object) {
                case Blackboard.PIEZA_P1:
						drawFicha(g, x, y, Color.RED);  
						break;
				case Blackboard.PIEZA_P2:
						drawFicha(g, x, y, Color.BLUE);  
						break;
            }
        }
		
        @Override
        public void drawAgent(Graphics g, int x, int y, Color c, int id) {
			// Por si se quiere visualizar el propio agente
		}

        public void drawFicha(Graphics g0, int x, int y, Color c) {
			Graphics2D g = (Graphics2D) g0;
			if (pos[x][y] < y) {
				pos[x][y]++;
			}
			g.setColor(c);
			
			int posx = 245 + x * 42 + 5;
			int posy = 165 + pos[x][y] * 42 + 5;
			
			if ((estado == ESTADO_FIN_JUEGO || estado == ESTADO_FIN_PARTIDA)
				&& isFichaGanadora(x, y)) {
				//g.setColor(Color.WHITE);
				g.setPaint(new GradientPaint(posx,
					posy, c, posx + 5, posy + 5, 
					new Color(220,220,220), true));
			} 
			g.fillOval(posx, posy, 42 - 10, 42 - 10);
        }
    }    
	
	/**
	 * Clase utilizada para controlar que ha pasado cierto tiempo
	 */ 
	class Timer {
		private double inicio;
		public Timer() {
			inicio = System.currentTimeMillis();
		}
		public void start() {
			inicio = System.currentTimeMillis();
		}
		public boolean cronometro(double tiempo) {
			return (System.currentTimeMillis() - inicio) > tiempo;
		}
	}
}



