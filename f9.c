typedef struct nodo {
int valor;
struct nodo *esq, *dir;
} * ABin;

ABin newABin (int r, ABin e, ABin d) {
    ABin a = malloc (sizeof(struct nodo));
    if (a!=NULL) {
        a->valor = r; a->esq = e; a->dir = d;
}
return a;
}

//exercicio 1 

//a) calcula a altura de uma arvore
int altura(ABin a){
    if(a==NULL) return 0;
  int altE= 1+ altura(a->esq);
  int altD=1+ altura(a->dir);
  if(altE>altD) return altE;
     else return altD;
    }
}

//b)calcula o nr de folhas 
int nFolhas (ABin a){
    if(a==NULL) return 0;
    else{
        return 1+ nFolhas(a->esq) + nFolhas(a->dir);
    }
}

//c) calcula o nodo mais a esquerda de uma arvore 
ABin maisEsquerda (ABin a){
    if(a==NULL) return NULL;
    while(a->esq!=NULL){
     a=a->esq;
    }
    return a ;
}

//d) escreve no ecra os elementos da arvore que estao num dado nivel
void imprimeNivel (ABin a, int l){
    if(l==0) printf("d\n",a->valor);
    else{
        imprimeNivel(a->esq,l-1);
        imprimeNivel(a->dir,l-1);
    }
}

//e) testa se um elemento ocorre na arvore
int procuraE(ABin a, int x){
    if(!a) return 0;
    if(a->valor == x) return 1;
    return procuraE(a->esq, x) || procuraE(a->dir, x);
}

//exercicio 2 arvore bin de procura

//f)procura um elemento numa arvore
struct nodo *procura (ABin a, int x){
    while(a != NULL && a->valor != x){
        if(a->valor < x) a = a->dir;
        else a = a->esq; 
    }
    return a;
}

//g)
int nivel (ABin a, int x){
    int ac = 0;
    while(a != NULL && a->valor != x){
        if(a->valor < x) a = a->dir;
        else a = a->esq; 
        ac++;
    }
    if(a == NULL) return -1;
    return ac;
}

//h)
void imprimeAte (ABin a, int x){
  if(a){
        imprimeAte(a->esq, x);
        if(x >= a->valor){
            printf("%d |", a->valor);
            imprimeAte(a->dir, x);
        }
    }
}
