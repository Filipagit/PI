#include <stdio.h>
#include <stdlib.h>

//1
int paresImpares(int v[],int N){
    int nump=N,i,j,tmp;
    for(i=0;i<N;i++){
        if(v[i]%2!=0) nump--;
        for(j=i+1;j<N;j++){
            if(v[j]%2==0){
                tmp=v[i];
                v[i]=v[j];
                v[j]=tmp;
                break;
            }
        }
        if(j==N) break;
    }
    return nump;
}

//2

typedef struct lint
{
    int valor;
    struct lint* prox;
}*LInt;

void merge(LInt *r,LInt a,LInt b){
  LInt *p = r;

    while(a || b) {
        if (!b || (a && a->valor < b->valor)) {
            *p=a;
            a=a->prox;
        }
        else {
            *p=b;
            b=b->prox;
        }
        p = &((*p)->prox);
    }
}

//3
void latino(int N,int m[N][N]){
    int i,j;
    for(i=0;i<N;i++){
        for(j=0;j<N;j++){
            if ((i+j)<N) m[i][j] = j+i+1;
      else m[i][j] = j+i+1-N;
        }
    }
} 

//4
typedef struct nodo {
int valor;
struct nodo *pai, *esq, *dir;
} *ABin;

ABin next (ABin a){
    if (!a) return NULL;
    else if (!a->esq && a->pai) return a->pai;
    else if (!a->esq && !a->pai) return next(a->dir);
    else return next(a->esq);
}

//5
typedef struct palavras{
    char* palavra;
    int nOcorr;
    struct palavras *esq, *dir;
} *Palavras;

int acrescentaPal(Palavras *p, char *pal) {
    Palavras *ant = malloc(sizeof(struct palavra *));
    *ant = NULL;
    int lado=0;


    while(*p && strcmp((*p)->palavra,pal)) {
        ant=p;
        if(strcmp((*p)->palavra, pal)<0) {p=&((*p)->dir); lado=0;} // Àrvore de procura -> Menores à esq, maiores à dir
        else {p=&((*p)->esq); lado=1;}
    }

    // Caso *p==NULL -> A palavra *pal não existe na árvore;    
    if(!*p) {
        Palavras new_p = malloc(sizeof(struct palavras));
        new_p->palavra = strdup(pal);
        new_p->nOcorr=1;
        new_p->dir = new_p->esq = NULL;
        *p=new_p;
    }

    // Existe e temos de verificar se vale a pena rodar a àrvore ou não;
    else {
        ((*p)->nOcorr)++;

        if((*ant) && (*ant)->nOcorr<(*p)->nOcorr) {
            if(lado) rodaDir(ant);
            else rodaEsq(ant);
        }
    }
    return (*p)->nOcorr;
}


int main(){
    int v[5] = {1,2,3,4,5};
    int pares = paresImpares(v, 5);
    for(int i=0; i<5; i++) {
        printf("%d ", v[i]);
    }
    printf("\n");
    printf("PARES: %d\n\n", pares);
}