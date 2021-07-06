typedef struct nodo {
int valor;
struct nodo *esq, *dir;
} * ABin;

//exercicio 1 arvore bin proc

//a) remove o nodo mais a esquerda de uma arvore
ABin removeMenor (ABin *a){
    if(*a==NULL) return NULL;
    while((*a)->esq!=NULL)
     (*a)=&(*a)->esq;
     Abin aux=*a;
     if ((*a)->dir) *a = (*a)->dir;
    return aux;
}

//b) remove a raiz de uma arvore nao vazia 
void removeRaiz (ABin *a){
    if(a!=NULL){
        Abin new= removeMenor(&(*a)->dir);
        new->esq=(*a)->esq;
        new->dir=(*a)->dir;
        *a=new;
    }
}

//c) remove um elemento
int removeElem (ABin *a, int x){
    if(a==NULL) return 1;
    if(x==(*a)->valor){
        removeRaiz(a);
        return 0;
    }
    else if(x<(*a)->valor) removeElem(&(*a)->esq,x);
         else removeElem(&(*a)->dir,x);
}

//exercicio 2 
void rodaEsquerda (ABin *a){
ABin b = (*a)->dir;
(*a)->dir = b->esq;
b->esq = (*a);
*a = b;
}

void rodaDireita (ABin *a){
ABin b = (*a)->esq;
(*a)->esq = b->dir;
b->dir = *a;
*a = b;
}

//d) que promove o menor elemento de uma arvore para o nıvel 0. 
void promoveMenor (ABin *a){
   if(*a && (*a)->esq){
       promoveMenor(&(*a)->esq);
       rodaDireita(a);
   }
}

//e)
void promoveMaior (ABin *a){
    if(*a && (*a)->dir){
       promoveMenor(&(*a)->dir);
       rodaEsquerda(a);
    }
}

//f)
ABin removeMenor2v(ABin *a){
    Abin r=NULL;
    promoveMenor(a);
    r=*a;
    *a = (*a)->dir;
    return r;
}

//exercicio 3 

//g)que transforma a arvore *a numa espinha. A funcao deve retornar o numero de nodos da arvore.
int constroiEspinhaAux(ABin *a, ABin *ult){
    int r = 0;
    if(*a){
        r = constroiEspinhaAux(&((*a)->esq), ult);
        rodaDireita(a);
        r = r + 1 + constroiEspinhaAux(&((*a)->dir), ult);
    }
}

int constroiEspinha(ABin *a){
    ABin ult;
    return(constroiEspinhaAux(a, &ult));
}