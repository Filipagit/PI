#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct celula {
char *palavra;
int ocorr;
struct celula * prox;
} * Palavras;

//1 liberta td o espaço ocupado por uma lista de palavras
void libertaLista (Palavras l){
    while(l!=NULL){
        Palavras tmp=l;
        l=l->prox;
        free(tmp);
    }
}

//2  conta o nr de pal dif lista nao tel pal rep conseguiii
int quantasP(Palavras l){
    int conta=0;
    while(l!=NULL){
     conta++;
     l=l->prox;
    }
    return conta;
} 

//3 escreve no ecra uma por linha tds as palavras armazenadas e o num de ocorrencias conseguiii 
void listaPal (Palavras l){
     while(l!=NULL){
         printf("%s\n",l->palavra,l->ocorr);
         l=l->prox;
     }
}

//4 calcula a ult palavra da lista 
char * ultima (Palavras l){
  if(l==NULL) return NULL;
  else{
      while(l->prox!=NULL){
          l=l->prox;
      }
  }
  return l->palavra;
}

//5 acrescenta uma palavra no inicio
Palavras acrescentaInicio (Palavras l, char *p){
    Palavras aux = malloc(sizeof(struct celula));
    aux->palavra=p;
    aux->ocorr=1;
    aux->prox=l;
    l=aux;
    return l;
}

//6 acrescenta no fim
Palavras acrescentaFim (Palavras l, char *p){
    Palavras aux=malloc(sizeof(struct celula));
    aux->palavra=p;
    aux->ocorr=1;
    aux->prox=NULL;
    if(l==NULL) l=aux;
    else{
        while(l->prox!=NULL){
            l=l->prox;
        }
        l->prox=aux;
    }
}
//7 adiciona mais uma ocorrencia da palavra p lista ordenada

Palavras acrescenta (Palavras l, char *p){
    int found = 0;
    Palavras *l1;
    for ( l1 = &l; *l1; l1 = &((*l1)->prox)){
        if (!strcmp (p,(*l1)->palavra)){
            found = 1;
            (*l1)->ocorr++;
            break;
        }
        if (strcmp (p,(*l1)->palavra) < 0) break; 
    }
    if (!found) {
        if (!*l1)l = acrescentaFim (l,p);
        else {
            Palavras new = malloc (sizeof (struct celula));
            new->ocorr = 1;
            new->palavra = p;
            new->prox = *l1;
            *l1 = new;
        }
    }
    return l;
}

//8
struct celula * maisFreq (Palavras l){
    struct celula biggest=l;
    while(l!=NULL){
        if(l->ocorr>biggest->ocorr) biggest=l;
        l=l->prox;
    }
    return biggest;
}


