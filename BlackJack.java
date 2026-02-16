package BlackJack;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class BlackJack {

    //Tamaño de la ventana
    int ancho = 700; 
    int alto = 600;

    //Tamaño de la carta
    int anchoCarta = 110;
    int altoCarta = 154;

    //Declaro los paneles
    JPanel botonPanel = new JPanel();
    JPanel apuestasPanel=new JPanel();

    //Declaro todos los botones
    JButton pedirBoton = new JButton("Pedir");
    JButton quedarBoton = new JButton("Quedarse");
    JButton reiniciarBoton = new JButton("Jugar otra mano");
    JButton apuesta1 = new JButton ("$100");
    JButton apuesta2 = new JButton ("$250");
    JButton apuesta3 = new JButton ("$500");
    
    //Variables del juego
    private Jugador jugador;
    private Jugador crupier;

    int apuestaRealizada=0;
    boolean apreto1=false;
    boolean apreto2=false;
    boolean apreto3=false;
    boolean mostrarCartas=false;
    boolean mostrarResultado=false;
    int plata=1000;
    Mazo mazo;
    boolean revelarCarta = false;

    JFrame frame = new JFrame("Black Jack - Mule");
    JPanel juegoPanel = new JPanel() {
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);
            try {
                //Establezco fondo
                Image fondo = new ImageIcon("C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/fondo.jpg").getImage();
                g.drawImage(fondo, 0, 0,getWidth(),getHeight(),null);

                //Establezco tipo de letra
                g.setFont(new Font ("Arial",Font.CENTER_BASELINE,15));
                g.setColor(Color.WHITE);

                //Muestro la cantidad de plata de inicio = 1000
                String plataTotal = "Plata en mano:"+plata;
                g.drawString(plataTotal, 15, 500); 
                
                if(mostrarCartas) //Si se realizo apuestas mostramos las cartas
                { 
                
                //Muestro los puntajes en el juego del crupier y del jugador
                String puntajeCrupier = revelarCarta ? "-Puntaje Crupier:"+crupier.getSum() : "Puntaje Crupier:?";
                g.drawString(puntajeCrupier,15,200);

                String puntajeJugador = "-Puntaje Jugador: "+jugador.getSum();
                g.drawString(puntajeJugador, 15, 460);

                
                if (revelarCarta) { // Se revelan si el jugador apretó el botón de quedarse
                    if (!crupier.getMano().isEmpty()) { // Verifica si hay al menos una carta en la mano del crupier
                        Carta cartaOculta = crupier.getMano().get(0);
                        Image imagenCartaOculta = new ImageIcon("C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/" + cartaOculta.getImagenFormato()).getImage();
                        g.drawImage(imagenCartaOculta, 20, 20, anchoCarta, altoCarta, null);
                    }
                } else {
                    // Mostramos la espalda de la primera carta del crupier
                    Image cartaOculta = new ImageIcon("C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/espalda.png").getImage();
                    g.drawImage(cartaOculta, 20, 20, anchoCarta, altoCarta, null);
                }
                
                // Mostramos las cartas del crupier (excepto la primera si no se han revelado)
                for (int i = 1; i < crupier.getMano().size(); i++) {
                    Carta carta = crupier.getMano().get(i);
                    Image imagenCarta = new ImageIcon("C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/" + carta.getImagenFormato()).getImage();
                    g.drawImage(imagenCarta, 20 + (anchoCarta + 10) * i, 20, anchoCarta, altoCarta, null);
                }
                
                // Mostramos las cartas del jugador
                for (int i = 0; i < jugador.getMano().size(); i++) {
                    Carta carta = jugador.getMano().get(i);
                    Image imagenCarta = new ImageIcon("C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/" + carta.getImagenFormato()).getImage();
                    g.drawImage(imagenCarta, 20 + (anchoCarta + 10) * i, 280, anchoCarta, altoCarta, null);
                }
             }
                // Mensaje de resultado
                if (mostrarResultado) {
                    String message = calcularResultado();
                    g.setFont(new Font("Arial", Font.CENTER_BASELINE, 30));
                    g.setColor(Color.white);
                    g.drawString(message, 220, 250);
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    };

    
    BlackJack() {
        iniciarJuego();

        frame.setSize(ancho, alto); 
        frame.setLocationRelativeTo(null); //Centrar  
        frame.setResizable(false); 
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        juegoPanel.setLayout(new BorderLayout());
        apuestasPanel.setLayout(new FlowLayout()); //Organizo los componenentes en una fila
        apuestasPanel.setOpaque(false);//Panel de apuestas transparente
        apuestasPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 10, 10)); //Lo acomodo a la derecha

        pedirBoton.setEnabled(false);
        quedarBoton.setEnabled(false);

        //Agrego botones
        botonPanel.add(pedirBoton);
        botonPanel.add(quedarBoton);
        botonPanel.add(reiniciarBoton);
        
        apuestasPanel.add(apuesta1);
        apuestasPanel.add(apuesta2);
        apuestasPanel.add(apuesta3);
        juegoPanel.add(apuestasPanel,BorderLayout.SOUTH); //Quedaria a la derecha abajo el panel de las apuestas
        frame.add(juegoPanel);
        frame.add(botonPanel, BorderLayout.SOUTH); //Botones de juego abajo

        // Acciones de los botones

        pedirBoton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jugador.getSum() >= 21) {
                    pedirBoton.setEnabled(false);
                    revelarCarta = true;
                } else {
                    Carta carta = mazo.sacarCarta();
                    jugador.AgregarCarta(carta);
                }
                frame.repaint();
            }
        });

        //Boton de quedar
        quedarBoton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                while (crupier.getSum() < 17) {
                    Carta carta = mazo.sacarCarta();
                    crupier.AgregarCarta(carta);
                }
                revelarCarta = true;
                mostrarResultado=true;
                pedirBoton.setEnabled(false);
                quedarBoton.setEnabled(false);
                frame.repaint();
            }
        });

        //Boton de la apuesta mas baja
        apuesta1.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                plata-=100;
                apuestaRealizada=100;
                apreto1=true;
                mostrarCartas=true;
                
                pedirBoton.setEnabled(true);
                quedarBoton.setEnabled(true);
                apuesta1.setEnabled(false);
                apuesta2.setEnabled(false);
                apuesta3.setEnabled(false);
                frame.repaint();
            }
        });

        //Boton de la apuesta 2
        apuesta2.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                plata-=250;
                apuestaRealizada=250;
                apreto2=true;
                mostrarCartas=true;
                pedirBoton.setEnabled(true);
                quedarBoton.setEnabled(true);
                apuesta1.setEnabled(false);
                apuesta2.setEnabled(false);
                apuesta3.setEnabled(false);
                frame.repaint();
            }
        });

        //Boton de la 3 apuesta
        apuesta3.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                plata-=500;
                apuestaRealizada=500;
                apreto3=true;
                mostrarCartas=true;
                
                pedirBoton.setEnabled(true);
                quedarBoton.setEnabled(true);
                apuesta1.setEnabled(false);
                apuesta2.setEnabled(false);
                apuesta3.setEnabled(false);
                frame.repaint();
            }
        });

        //Boton de reiniciar
        reiniciarBoton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                iniciarJuego();
                pedirBoton.setEnabled(false);
                quedarBoton.setEnabled(false);
                apuesta1.setEnabled(true);
                apuesta2.setEnabled(true);
                apuesta3.setEnabled(true);
                apuestaRealizada=0;
                revelarCarta=false;
                mostrarCartas=false;
                mostrarResultado=false;
                frame.repaint();
            }
        });
        frame.setVisible(true);
    }

    private void iniciarJuego() {
        mazo = new Mazo();
        mazo.shuffle();
        jugador = new Jugador();
        crupier = new Jugador();

        // Repartir cartas iniciales
        jugador.AgregarCarta(mazo.sacarCarta());
        jugador.AgregarCarta(mazo.sacarCarta());
        crupier.AgregarCarta(mazo.sacarCarta());
        crupier.AgregarCarta(mazo.sacarCarta());

    }

    //Calcular resultado
    private String calcularResultado() {
        
        //Se paso el jugador
        if (jugador.getSum() > 21 || jugador.getSum() < crupier.getSum() && crupier.getSum()<= 21) {
            
            apreto1=false;
            apreto2=false;
            apreto3=false;
            apuestaRealizada=0;

            return "Perdiste";
        }

        //Si el crupier se pasa o si el jugador tiene mayor puntaje
        if (crupier.getSum() > 21 || jugador.getSum() > crupier.getSum()) {
            if (apreto1 || apreto2 || apreto3) {
                plata += apuestaRealizada * 2;
                System.out.println(plata);
        
                //Reinicio estados
                apreto1 = false;
                apreto2 = false;
                apreto3 = false;
        
                frame.repaint();
            }
            return "Ganaste!";
        }
        return "Empate!";
    }
}
