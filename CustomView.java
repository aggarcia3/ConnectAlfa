import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import java.util.logging.Logger;
import java.awt.image.BufferedImage;
import java.awt.geom.AffineTransform;

import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.ImageIcon;

import java.awt.RenderingHints;

import javax.swing.JFrame;

/**
 * View component for a GirdWorldModel. 
 * 
 * @author Miguel Callon
 */
 public abstract class CustomView extends JFrame {
	static Logger logger = Logger.getLogger(CustomView.class.getName());

    private static final long serialVersionUID = 1L;

    protected int cellSizeW = 0;
    protected int cellSizeH = 0;

	protected Image dobleBuffer;
    protected DefaultCanvas     drawArea;
    protected CustomModel model;
	
	protected int estado = 0;
	protected final int ESTADO_FIN_JUEGO = 1;
	protected final int ESTADO_FIN_PARTIDA = 2;
	private FinPartida finPartida;
	private int ganador;
	private final int P1_GANADOR = 5;
	private final int P2_GANADOR = 6;
	private final int EMPATE = 10;
	
	private int p1Score;
	private int p2Score;
	private int empates;
	private int rondas;
	private int jugadorTurnoInicial;
	private final int TURNO_INICIAL_P1 = 1;
	private final int TURNO_INICIAL_P2 = 2;
	
    protected Font defaultFont = new Font("Arial", Font.BOLD, 10);
 
	public Personaje personaje1, personaje2;
	public Sombra sombra1, sombra2;
	public ImageIcon fondo[];
	public ImageIcon fondoFinal;
	public int horizontalScroll;
	
	// Modos de juego
	private int modoJuego = 0;
	private int MODO_JUGAR_A_GANAR = 1;
	private int MODO_JUGAR_A_PERDER = 2;
	
    public CustomView(CustomModel model, String title, int windowSize) {
        super(title);
 
		this.model = model;
        initComponents(windowSize);
        model.setView(this);
		
		Font font = new Font("Serif", Font.BOLD, 23);
		Font font2 = new Font("Serif", Font.BOLD, 23);
		TextoPersonaje textoPersonaje1 = new TextoPersonaje(
			"PICA PICA!!", font, Color.YELLOW, 0, 230);
		TextoPersonaje textoPersonaje2 = new TextoPersonaje(
			"CHARMANDER!!", font2, Color.RED, 595, 260);
		
		personaje1 = new Personaje("personaje1.png", textoPersonaje1, 0, 230);
		personaje2 = new Personaje("personaje2.png",  textoPersonaje2, 595, 260);
		sombra1 = new Sombra(80, 485);
		sombra2 = new Sombra(600, 485);
		
		finPartida = new FinPartida();
		
		fondo = new ImageIcon[2];
		fondo[0] = new ImageIcon("fondo.png"); 
		fondo[1] = new ImageIcon("supercombofinish.gif");
		fondoFinal = new ImageIcon();
		horizontalScroll = 0;
	}
	
	public void setModoJuego(int modoJuego) {
		this.modoJuego = modoJuego;
	}
	
	public void setJugadorTurnoInicial(int jugadorTurnoInicial) {
		this.jugadorTurnoInicial = jugadorTurnoInicial;
	}
	
	public void setP1Score(int p1Score) {
		this.p1Score = p1Score;	
	}
	
	public void setP2Score(int p2Score) {
		this.p2Score = p2Score;
	}
	
	public void setEmpates(int empates) {
		this.empates = empates;
	}
	
	public void setRonda(int rondas) {
		this.rondas = rondas;
	}
	
	public void setFinPartida(int ganador) {
		estado = ESTADO_FIN_PARTIDA;
		this.ganador = ganador;
	}
	
	public void setFinJuego(int ganador) {
		estado = ESTADO_FIN_JUEGO;
		this.ganador = ganador;
	}
	
	public void setNuevaPartida() {
		estado = 0;
		this.ganador = 0;
	}

    /** sets the size of the frame and adds the components */
    public void initComponents(int width) {
		setSize(833,585);
		setBackground(Color.black);
        getContentPane().setLayout(new BorderLayout());
        drawArea = new DefaultCanvas();
        getContentPane().add(BorderLayout.CENTER, drawArea);        
    }
	
	public DefaultCanvas getDrawArea() {
		return drawArea;
	}

    /** updates all the frame */
    public void update() {
        repaint();
    }
	
	private BufferedImage imageToBufferedImage(Image im) {
		BufferedImage bi = new BufferedImage(im.getWidth(null),
				im.getHeight(null), BufferedImage.TYPE_INT_RGB);
		Graphics bg = bi.getGraphics();
		bg.drawImage(im, 0, 0, null);
		bg.dispose();
		return bi;
	}
	
	public void drawFondo(Graphics2D g) {
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON);
		
		BufferedImage bufferedImage = null;;
		BufferedImage resultado = null;
			
		if (estado != ESTADO_FIN_JUEGO) {
			bufferedImage = imageToBufferedImage(fondo[0].getImage());
			int matriz[][] = new int[bufferedImage.getWidth()][bufferedImage
					.getHeight()];
					
			for (int i = 0; i < bufferedImage.getWidth(); i++) {
				for (int j = 0; j < bufferedImage.getHeight(); j++) {
					matriz[i][j] = bufferedImage.getRGB(i, j);
				}
			}
			
			resultado = new BufferedImage(bufferedImage.getWidth(),
						bufferedImage.getHeight(), BufferedImage.TYPE_INT_RGB); 
	
			for (int i = 0; i < bufferedImage.getWidth() - horizontalScroll; i++) {
				for (int j = 0; j < bufferedImage.getHeight(); j++) {
					resultado.setRGB(horizontalScroll + i, j, matriz[(bufferedImage.getWidth()-1) - i][j]);
				}
			}
			for (int i = 0; i < horizontalScroll; i++) {
				for (int j = 0; j < bufferedImage.getHeight(); j++) {
					resultado.setRGB(i, j, matriz[(bufferedImage.getWidth()) - horizontalScroll + i][j]);
				}
			}		
			horizontalScroll++;
			if (horizontalScroll >= bufferedImage.getWidth()) horizontalScroll=0;
			
			fondoFinal.setImage(new ImageIcon(resultado).getImage());
		} else {
			fondoFinal = fondo[1];
		}
		
		g.drawImage(fondoFinal.getImage(),
					0, 52, this);
					
		// Dibujo player 1 y player 2
		Font font = new Font("Serif", Font.BOLD, 50);
		g.setFont(font);
		g.setColor(new Color(0,0,0,140));
		g.fillRect(0,51,850,50);
		g.setColor(Color.BLACK);
		g.drawString("PLAYER1", 8, 90);
		g.drawString("PLAYER2", 580, 90);
		g.setFont(font);
		g.setColor(Color.RED);
		g.drawString("PLAYER1", 10, 92);
		g.setColor(Color.BLUE);
		g.drawString("PLAYER2", 582, 92);
		g.setColor(Color.BLACK);
		g.drawString("VS", 368, 90);
		g.setColor(Color.WHITE);
		g.drawString("VS", 370, 93);
		g.setColor(Color.WHITE);
		font = new Font("Serif", Font.PLAIN, 15);
		g.setFont(font);
		g.drawString("Sistemas Inteligentes (SI) - Curso 2018/2019", 545, 30);
		g.drawString("Créditos: Miguel Callon - Curso 2013/2014", 545, 125);
		
		sombra1.dibujar(g);
		sombra2.dibujar(g);
		personaje1.dibujar(g);
		personaje2.dibujar(g);
		
		Rectangle2D rbg = new Rectangle2D.Float();
		g.setColor(new Color(255,255,255,180));
		rbg.setFrame(245, 165, 336, 336);
		g.fill(rbg);

		Rectangle2D r = new Rectangle2D.Float();
		r.setFrame(245, 165, 336, 336);
		Area a1 = new Area(r);
		
		for(int filas = 0; filas < 8; filas++) {
			for(int cols = 0; cols < 8; cols++) {
				Ellipse2D e = new Ellipse2D.Double();
				e.setFrame(245 + cols * 42 + 5,  165 + filas * 42 + 5, 42 - 10, 42 - 10);
				Area a2 = new Area(e);
				a1.subtract(a2);
			}
		}

		g.setPaint(new GradientPaint(0,0, Color.blue, 300,300, new Color(100,100,255), true));
		g.fill(a1);
	
		Rectangle2D rSombra = new Rectangle2D.Float();
		g.setPaint(new GradientPaint(0,0, new Color(0,0,255,150), 0,15, new Color(0,0,0,100), true));
		rSombra.setFrame(245, 511, 336, 15);
		g.fill(rSombra);		
	}

    public void drawObstacle(Graphics g, int x, int y) {
        g.setColor(Color.darkGray);
        g.fillRect(x * cellSizeW + 1, y * cellSizeH+1, cellSizeW-1, cellSizeH-1);
        g.setColor(Color.black);
        g.drawRect(x * cellSizeW + 2, y * cellSizeH+2, cellSizeW-4, cellSizeH-4);
    }

    public void drawAgent(Graphics g, int x, int y, Color c, int id) {
        g.setColor(c);
        g.fillOval(x * cellSizeW + 2, y * cellSizeH + 2, cellSizeW - 4, cellSizeH - 4);
        if (id >= 0) {
            g.setColor(Color.black);
            drawString(g, x, y, defaultFont, String.valueOf(id+1));
        }
    }
    
    public void drawString(Graphics g, int x, int y, Font f, String s) {
        g.setFont(f);
        FontMetrics metrics = g.getFontMetrics();
        int width = metrics.stringWidth( s );
        int height = metrics.getHeight();
        g.drawString( s, x*cellSizeW+(cellSizeW/2-width/2), y*cellSizeH+(cellSizeH/2+height/2));
    }

    public void drawEmpty(Graphics g, int x, int y) {
        g.setColor(Color.white);
        g.fillRect(x * cellSizeW + 1, y * cellSizeH+1, cellSizeW-1, cellSizeH-1);
        g.setColor(Color.lightGray);
        g.drawRect(x * cellSizeW, y * cellSizeH, cellSizeW, cellSizeH);
    }

    /** method to draw unknown object, probably overridden by the user viewer class */
    public abstract void draw(Graphics g, int x, int y, int object);/* {
        //g.setColor(Color.black);
        //drawString(g,x,y,defaultFont,String.valueOf(object));
    }*/

    private static int limit = (int)Math.pow(2,14);

    private void draw(Graphics g, int x, int y) {        
		for (int i = 0; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				draw(g, i, j, model.data[i][j]);
			}
		}
    }
    
    public Canvas getCanvas() {
        return drawArea;
    }
    
    public CustomModel getModel() {
        return model;
    }
	
	class DefaultCanvas extends Canvas {
        
		private static final long serialVersionUID = 1L;
	
		//Image dobleBuffer = 
		public DefaultCanvas() {
			super();
		}
		
		public void update(Graphics g2) {
			dobleBuffer = createImage(833,550);
			Graphics2D g = (Graphics2D) dobleBuffer.getGraphics();
			if (g == null) return;
			
			g.setColor(Color.BLACK);
			g.fillRect(0,0,833,550);
			
			// Dibujo el fondo
			drawFondo(g);
			
			// Dibujo los elementos
			draw(g, 0, 0);
			
			if (estado == ESTADO_FIN_JUEGO || estado == ESTADO_FIN_PARTIDA) {
				drawFinal(g);
			}
			
			drawScore(g);
			
			// Dibujo todo
			g2.drawImage(dobleBuffer, 0, 0, this);
		}
	}
	
	/**
	 * Dibujamos la puntuacion
	 */
	void drawScore(Graphics g0) {
		Graphics2D g = (Graphics2D) g0;
		Font font = new Font("Serif", Font.BOLD, 15);
		g.setFont(font);
		
		if (jugadorTurnoInicial == TURNO_INICIAL_P1) {
			g.setColor(Color.RED);
			g.fillOval(10,20,10,10);
		} else {
			g.setColor(Color.BLUE);
			g.fillOval(70,20,10,10);
		}
			
		g.setColor(Color.WHITE);
		StringBuffer sb = new StringBuffer();
		sb.append("P1: ").append(p1Score).append("      P2: ").append(p2Score).
			append("      Ronda: ").append(rondas).append("   Modo: ");
			
		if (modoJuego == MODO_JUGAR_A_GANAR) {
			sb.append("JUGAR A GANAR");
		} else {
			sb.append("JUGAR A PERDER");
		}
			
		g.drawString(sb.toString(), 25, 30);
	}
	
	void drawFinal(Graphics g) {
		finPartida.pintar(g);
	}
	
	class FinPartida {
		private int cont;
		public FinPartida() {
			cont = 0;
		}
		
		public void startFinPartida() {
			cont = 0;
		}
		
		public void pintar(Graphics g0) {
			Graphics2D g = (Graphics2D) g0;
			Font font = new Font("Serif", Font.BOLD, 55);
			g.setFont(font);
			
			int numJugador = 0;
			if (ganador == P1_GANADOR) {
				numJugador = 1;
			} else if (ganador == P2_GANADOR) {
				numJugador = 2;
			}
			
			if (ganador == EMPATE) {
				g.setColor(new Color(0,0,0));
				g.drawString("DEUCE!!", 298, 268);
				g.setColor(new Color(255,0,0));
				g.drawString("DEUCE!!", 300, 270);
			} else {
				g.setColor(new Color(0,0,0));
				g.drawString("PLAYER " + numJugador + " WIN!!", 200, 270);
				g.setColor(new Color(255,0,0));
				g.drawString("PLAYER " + numJugador + " WIN!!", 198, 268);	
			}
		}
	}
	
	class TextoPersonaje {
		private String texto;
		private Font f;
		private Color c;
		private int x,y;
		public TextoPersonaje(String texto, Font f, Color c, int x, int y) {
			this.texto = texto;
			this.f = f;
			this.c = c;
			this.x = x;
			this.y = y;
		}
		
		public void pintar(Graphics g) {
			Graphics2D g2 = (Graphics2D)g;
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON);
			
			g2.setFont(f);
		
			g.setColor(Color.BLACK);
			g2.drawRoundRect(x + 20, y - 80, 200, 75,25,25);
			g.setColor(new Color(0,0,0,150));
			g2.fillRoundRect(x + 20, y - 80, 200, 75,25,25);
			g2.setColor(c);
			
			g2.drawString(texto, x + 30, y - 30);
		}
	}
	
	class Personaje {
		int x,y, xTmp, yTmp, estado;
		int yVibrar, ySentido, cont;
		TextoPersonaje texto;
		ImageIcon image;
		
		public Personaje(String file, TextoPersonaje texto, int x, int y) {
			image = new ImageIcon(file);
			this.x = x;
			this.y = y;
			this.xTmp = x;
			this.yTmp = y;
			this.estado = 0;
			this.yVibrar = 10;
			this.ySentido = 0;
			this.cont = 0;
			this.texto = texto;
		}
		
		public void vibrar() {
			estado = 1;
		}
		
		public void dibujar(Graphics g) {
			if (estado == 1) {
				// Tiene que vibrar
				if (ySentido == 0) {
					y += yVibrar;
					ySentido = 1;
				} else {
					y -= yVibrar;
					ySentido = 0;
				}	
				
				cont++;
				if (cont == 5) {
					cont = 0;
					estado = 0;
					x = xTmp;
					y = yTmp;
				}
				texto.pintar(g);
			}

			g.drawImage(image.getImage(), x, y, null);
		}
	}
	
	class Sombra {
		int x,y;
		
		public Sombra(int x, int y) {
			this.x = x;
			this.y = y;
		}
		
		public void dibujar(Graphics g) {
			g.setColor(new Color(255,255,255,100));
			g.fillOval(x,  y, 130, 42);
		}
	}
}


