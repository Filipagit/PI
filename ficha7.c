#include <stdio.h>
#include <stdlib.h>

typedef struct celula {
    char *palavra;
    int ocorr;
    struct celula * prox;
} * Palavras;


void libertaLista (Palavras l){
      while(l!=NULL){
        Palavras tmp=l;
        l=l->prox;
        free(tmp);
    }
}

int quantasP (Palavras l){
      int q=0;
    while(l!=NULL){
        q++;
        l=l->prox;
    }
    return q;
}

void listaPal (Palavras l){
     while(l!=NULL){
        printf("%s (%d)/n",l->palavra,l->ocorr);
        l=l->prox;
    }
}
char * ultima (Palavras l){
if(l==NULL) return NULL;
  else{
      while(l->prox!=NULL){
          l=l->prox;
      }
  }
  return l->palavra;
}
Palavras acrescentaInicio (Palavras l, char *p){
   Palavras i=malloc(sizeof(struct celula));
    i->palavra=p;
    i->ocorr=1;
    i->prox=l;
    l=i;
    return l;
}
Palavras acrescentaFim (Palavras l, char *p){
    Palavras f=malloc(sizeof(struct celula));
    f->palavra=p;
    f->ocorr=1;
    f->prox=NULL;
    if(l==NULL) l=f;
    else{
        while(l->prox!=NULL) l=l->prox;
        l->prox=f;
    }
}
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
struct celula * maisFreq (Palavras l){
 struct   celula *biggest=l;
    while(l!=NULL){
        if(l->ocorr > biggest->ocorr) biggest=l;
        l=l->prox;
    }
    return biggest;

}

int main () {
    Palavras dic = NULL;

    char * canto1 [44] = {"as", "armas", "e", "os", "baroes", "assinalados",
                          "que", "da", "ocidental", "praia", "lusitana", 
                          "por", "mares", "nunca", "de", "antes", "navegados",
                          "passaram", "ainda", "alem", "da", "taprobana",
                          "em", "perigos", "e", "guerras", "esforcados",
                          "mais", "do", "que", "prometia", "a", "forca", "humana",
                          "e", "entre", "gente", "remota", "edificaram", 
                          "novo", "reino", "que", "tanto", "sublimaram"};

    printf ("\n_____________ Testes _____________\n\n");

    int i; struct celula *p;
    for (i=0;i<44;i++)
        dic = acrescentaInicio (dic, canto1[i]);

    printf ("Foram inseridas %d palavras\n", quantasP (dic));
    printf ("palavras existentes:\n");
    listaPal (dic);
    printf ("última palavra inserida: %s\n", ultima (dic));

   libertaLista (dic);

    dic = NULL;

    srand(42);
    
    for (i=0; i<1000; i++)
        dic = acrescenta (dic, canto1 [rand() % 44]);
    
    printf ("Foram inseridas %d palavras\n", quantasP (dic));
    printf ("palavras existentes:\n");
    listaPal (dic);
    printf ("última palavra inserida: %s\n", ultima (dic));
    
    p = maisFreq (dic);
    printf ("Palavra mais frequente: %s (%d)\n", p->palavra, p->ocorr);
    
    printf ("\n_________ Fim dos testes _________\n\n");

    return 0;
}