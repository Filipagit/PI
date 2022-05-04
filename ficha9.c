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

//Exercicio 1
//a
int altura(ABin a){
    int e,d;
    if(a==NULL) return 0;
    e=1+altura(a->esq);
    d=1+altura(a->dir);
    if(e>d) return e;
    else return d;
}

//b
int nFolhas (ABin a){
    int n=0;
    if(a!=NULL){
        if(a->esq==NULL && a->dir==NULL) n++;
        n+=nFolhas(a->esq)+nFolhas(a->dir);
    }
    return n;
}

//c
ABin maisEsquerda (ABin a){
    if(a==NULL) return NULL;
    while(a->esq!=NULL) a=a->esq;
   return a;
}

//d
void imprimeNivel (ABin a, int l){
    if(a!=NULL && n>0){
        if(n==1) printf("%d",a->valor);
        else{
            imprimeNivel(a->esq,n-1);
            imprimeNivel(a->dir,n-1);
        }
    }
}

//e
int procuraE (ABin a, int x){
    if(a==NULL) return 0;
    if(x==a->valor) return 1;
    if(procuraE(a->esq,x)) return 1;
    return(procuraE(a->dir,x));
}

//alternativa
int procuraE (ABin a, int x){
    return(a!=NULL &&(a->valor==x || procuraE(a->esq,x)||procuraE(a->dir,x)));
}

//2 Arvores binarias de procura
//f
struct nodo *procura (ABin a, int x){
    if(a!=NULL && a->valor!=x){
        if(a->valor<x)  a=a->dir;
        else a=a->esq;
    }
    return a;
}

//g
int nivel (ABin a, int x){
    int r;
    if(a==NULL) return 0;
    if(x==a->valor) return 1;
    if(a->valor<x) r=nivel(a->esq,x);
    else r=nivel(a->dir,x);
    if(r!=0) r=r+1;
    return r;
}

//alternativa
int nivel (ABin a, int x){
    int n = 1;
    while(a != NULL && a->valor != x){
        n++;
        if(x<a->valor ) a = a->esq;
        else a = a->dir; 
    }
    if(a == NULL) return 0;
    else return n;
}

//h
 void imprimeAte (ABin a, int x){
     if(a!=NULL){
         imprimeAte(a->esq,x);
         if(a->valor<x){
             printf("%d\n",a->valor);
             imprimeAte(a->dir,x);
         }
     }
 }