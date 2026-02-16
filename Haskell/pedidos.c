#include <stdio.h>
#include <stdlib.h>

#define  MAX 100

void limpiarBuffer() {
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
}
struct nodo{
    int numPedi;
    char nombrePedi[MAX];
    struct nodo *sig;
};
struct nodo *head=NULL;
struct nodo *tail=NULL;

int vacia()
{
    return head==NULL;
}

void insertar(struct nodo *nuevo)
{
    if(vacia())
    {
        head=nuevo;
        tail=nuevo;
    }
    else
    {
        tail->sig=nuevo;
        tail=nuevo;
    }
}

void cargar()
{
    struct nodo *pedido;
    pedido=malloc(sizeof(struct nodo));
    pedido->sig=NULL;
    printf("Ingresa el numero de pedido:");
    scanf("%d",&pedido->numPedi);
    limpiarBuffer();
    printf("Ingresa el producto:");
    fgets(pedido->nombrePedi,MAX,stdin);
    insertar(pedido);
}

void mostrarPedidos()
{
    struct nodo *reco=head;
    printf("PEDIDOS A ENTREGAR\n");
    while(reco!=NULL)
    {
        printf("Num pedido:%d   -   Pedido:%s\n",reco->numPedi,reco->nombrePedi);
        reco=reco->sig;
    }
    printf("\n");
}

int main()
{
    cargar();
    mostrarPedidos();
    return 0;
}