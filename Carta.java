
package BlackJack;

public class Carta{
    //Atributos
    private String valor;
    private String palo;

    //Metodos
    public Carta(String valor,String palo)
    {
        this.valor=valor;
        this.palo=palo;
    }

    //Devuelvo valor correspondiente a la carta
    public int getValor()
    {
        switch (valor) {
            
            case "A":
            return 11;

            case  "J":
            case  "Q":
            case  "K":
            return 10;
            default:
            return Integer.parseInt(valor);
        }
    }

    //Verifico si es un AS osea si es "A"
    public boolean esAS()
    {
        return valor.equals("A");
    }

    //Devuelvo la ruta donde se encuentra la carta
    public String getImagen()
    {
        return "C:/Users/Maximo Mule/OneDrive/Escritorio/CartasJuego/" + getImagenFormato();
    }

    public String getImagenFormato()
    {
        return palo + "-" + valor + ".jpg";
    }

}