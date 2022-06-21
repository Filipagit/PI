#include <stdio.h>
#include <stdlib.h>


//1
void insereOrd(int v[],int num,int N){
    int f=0;
    int i,j;
    for(i=0;i<N;i++){
        if(num<v[i]){
            for(j=N;j>i;j--)
            v[j]=v[j-1];
            v[i]=num;
        }
        if(num==v[i]) return;
    }
    if(!f) v[N]=num;
}

int sumhtpo (int n) {
  int a[100];
  int N=0;
   while (n != 1) {
       insereOrd(a,n,N++);
    if (n%2 == 0) n = n/2; 
    else n = 1+(3*n);
    if(N>100) N=100;
   }
   
   if(N<100) return -1;
   return a[99];
}

//2
int frequencia(int v[],int num,int N){
    int i,freq=0;
    for(i=0;i<N;i++){
        if(v[i]==num) freq++;
    }
    return freq;
}

int moda(int v[], int N, int *m){
    int i,freq=0,maisfreq=0,repetido=0;
    if(N==0) return 0;
    for(i=0;i<N;i++){
        freq=frequencia(v,v[i],N);
        if(freq>maisfreq){
            maisfreq=freq;
            *m=v[i];
            repetido=0;
        }
       else if(freq==maisfreq) repetido=1; 
    }
    if(repetido ==1) return 0;
    return maisfreq;
}

typedef struct lint
{
    int valor;
    struct lint* prox;
}*LInt;

int procura (LInt *l, int x) {
    int *r=l;
    while(*r){
        if((*r)->valor==x){
            LInt tmp=*r;
            *r=(*r)->prox;
            tmp->prox=*l;
            *l=tmp;
            return 1;
        }
        r=&((*r)->prox);
    }
    return 0;
}

//4
typedef struct nodo {
int valor;
struct nodo *pai, *esq, *dir;
} *ABin;

int freeAB(ABin a){
    if(!a) return 0;
    int e=freeAB(a->esq);
    int d=freeAB(a->dir);
    free(a);
    return 1+e+d;
}


//5

void caminho(ABin a){
    int v[100],N=0;
    int i;
    while(a){
        ABin ant=a;
        a=a->pai;
        if(ant==a->dir) v[N++]=0;
        else v[N++]=1;
    }
    for(i=N-1;i>=0;i--){
        if(v[i]==0)  printf("> dir\n");
    else printf(" > esq\n");
    }
}




int main(){
    //Ex1
    int ult=sumhtpo(500);
    printf("Ex1: %d\n",ult);
    //Ex2
    int m;
    int v1[5] = {1,2,3,4,5};
    int v2[5] = {1,1,2,2,3};
    int v3[7] = {1,1,2,2,3,3,3};
    int r1 = moda(v1,5,&m);
    printf(" > 1. %d \n", r1);
    int r2 = moda(v2,5,&m);
    printf(" > 2. %d \n", r2);
    int r3 = moda(v3,7,&m);
    printf(" > 3. %d \n\n", r3);

    //Ex 5
     ABin b = malloc(sizeof(struct nodo));
    b->pai=NULL;
    b->valor = 1;
    b->esq = malloc(sizeof(struct nodo));
    b->esq->pai = b;
    b->esq->valor = 2;
    b->dir = malloc(sizeof(struct nodo));
    b->dir->pai = b;
    b->dir->valor = 3;

    caminho(b->dir);

       return 0;

}