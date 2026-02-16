package BlackJack;

import java.util.ArrayList;

public class Jugador {
    private ArrayList<Carta> mano; //Cartas del jugador
    private int puntaje;
    private int asCuenta; //Conteo de las "A"

    public Jugador()
    {
        this.mano = new ArrayList<>();
        this.puntaje = 0;
        this.asCuenta=0;
    }

    //Agregamos carta a la mano
    public void AgregarCarta(Carta carta)
    {
        mano.add(carta);
        puntaje += carta.getValor();
        if(carta.esAS())
        {
            asCuenta++;
        }
    }

    public int getSum()
    {
        while(puntaje>21 && asCuenta > 0)
        {
            puntaje-=10;
            asCuenta--;
        }
        return puntaje;
    }


    public ArrayList <Carta> getMano()
    {
        return mano;
    }

    public void limpiar()
    {
        mano.clear();
        puntaje=0;
    }
    
}
