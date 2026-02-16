package BlackJack;

import java.util.ArrayList;
import java.util.Collections;

public class Mazo {
    private ArrayList<Carta> mazo;

    public Mazo() {
        this.mazo=new ArrayList<>();
        construirMazo();
        }
        
        private void construirMazo()
        {
        mazo = new ArrayList<>();
        String[] palos = {"D", "T", "P", "C"};
        String[] valores = {"2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"};

        for (String palo : palos) {
            for (String valor : valores) {
                Carta carta = new Carta(valor, palo);  
                mazo.add(carta);
            }
        }
        shuffle();  // Mezclar las cartas
    }

    public void shuffle() {
        Collections.shuffle(mazo);  
    }

    public Carta sacarCarta() {
        return mazo.remove(mazo.size() - 1);  
    }

}