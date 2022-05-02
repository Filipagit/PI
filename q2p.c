#include <stdio.h>
#include <stdlib.h>

typedef struct lligada {
int valor;
struct lligada *prox;
} *LInt;

typedef struct nodo {
int valor;
struct nodo *esq, *dir;
} *ABin;

//1 calcula o comprimento de uma lista ligada
int length(LInt l){
   int size=0;
    while(l!=NULL){
         size++;
         l=l->prox;
    } 
    return size;
}

//2 liberta o espaço ocupado por uma lista 
void freeL(LInt l){
     LInt tmp;
     while(l!=NULL){
         tmp=l;
         l=l->prox;
         free(tmp);
     }
}

//3 imprime no ecra todos os elementos da lista
void imprimeL(LInt l){
    while(l!=NULL){
        printf("%d\n",l->valor);
        l=l->prox;
    }
}

//4
LInt reverseL(LInt l){
    LInt tmp;
    LInt anterior=NULL;
    while(l!=NULL){
        tmp=l->prox;
        l->prox=anterior;
        anterior=l;
        l=tmp;
    }
    return anterior;
}

//5 
void insertOrd (LInt *l, int x){
     LInt new;
    while((*l)!=NULL && (*l)->valor<x) l=&((*l)->prox);
       new=malloc(sizeof(struct lligada));
       new->valor=x;
       new->prox=*l;
       *l=new;
}

//6
int removeOneOrd (LInt *l, int x){
    while((*l)!=NULL && (*l)->valor!=x) l=&((*l)->prox);
    if((*l)!=NULL){
    LInt tmp;
      tmp=(*l)->prox;
      free(*l);
      *l=tmp;
      return 0;
    }
    else return 1;
}
//7 junta duas listas ordenadas numa 
void merge (LInt *r, LInt l1, LInt l2){
    LInt novo;
    while(l1!=NULL || l2!=NULL){
        if(l1==NULL || l2!=NULL && l1->valor > l2->valor){
           novo=malloc(sizeof(struct lligada));
           novo->valor=l2->valor;
           novo->prox=NULL;
           *r=novo;
           r=&(novo->prox);
           l2=l2->prox;
        }
        else{
            novo=malloc(sizeof(struct lligada));
           novo->valor=l1->valor;
           novo->prox=NULL;
           *r=novo;
           r=&(novo->prox);
           l1=l1->prox;
        }
    }
}

//8 
void splitQS (LInt l, int x, LInt *mx, LInt *Mx){
        while(l!=NULL){
        if(l->valor >= x){
        *Mx =l;
        Mx=&((*Mx)->prox);
    }
    else{
        *mx = l;
        mx=&((*mx)->prox);
    }
    l=l->prox;
    }
    *mx=0;
    *Mx=0;
}

//9
LLig parteAmeio (LLig *l){
     int i, n = length(*l)/2;
	LInt *p = l, u = *l;

	for(i=0; i<n; i++) p = &((*p)->prox);

	*l = *p;
	*p = NULL;

	if(n == 0) {
		*l = u;
		u = NULL;
	}
	return u;
}
//10
int removeAll (LInt *l, int x){
     int rem=0;
  while((*l)!=NULL){
      if((*l)->valor == x){
      LInt tmp=(*l)->prox;
      free(*l);
      *l=tmp;
      rem++;   
      } else l=&((*l)->prox);
  }
  return rem;
}

//11 remove elementos repetidos de uma lista deixa apenas a 1º ocorrencia
//calcula o nr de ocorrencias de um inteiro numa lista
int removeDups (LInt *l){
  int rem = 0;

	while((*l)!=NULL) {
		LInt ant = *l;
		LInt px = (*l)->prox;

		while(px!=NULL) {
			if(px->valor == (*l)->valor) {
				ant->prox = px->prox;
				rem++;
			}
			else ant = px;
			px = ant->prox;
		}
		l = &(*l)->prox;
	}
	return rem;
}


//12
int removeMaiorL (LInt *l){
     int maior;
   LInt * p,t;
   for (p = l;*p; p=&((*p)->prox))
       if((*l)->valor<(*p)->valor)
           l=p;
   maior=(*l)->valor;
   t=(*l)->prox;
   free(*l);
   *l=t;
   return maior;
}

//13
void init (LInt *l){
    while((*l)->prox!=NULL) l=&((*l)->prox);
    LInt tmp=(*l)->prox;
    free(*l);
    *l=tmp;
}

//14
void appendL (LInt *l, int x){ 
    LInt novo=malloc(sizeof(struct lligada));
    novo->valor=x;
    novo->prox=NULL;
    if((*l)==NULL) (*l)=novo;
    else{
        while((*l)->prox!=NULL) l=&((*l)->prox);
    (*l)->prox=novo;
    }
}
//15 acrescenta b à lista *a
void concatL (LInt *a, LInt b){
     while((*a)!=NULL) a=&((*a)->prox);
     (*a)=b;
}
//16
LInt cloneL (LInt l){
    if(l==NULL) return NULL;
        LInt novo=malloc(sizeof(struct lligada));
        novo->valor=l->valor;
        novo->prox=cloneL(l->prox);
    return novo;
}
//17
LInt cloneRev (LInt l){
  LInt r = NULL;

	while(l!=NULL) {
		LInt novo = malloc(sizeof(struct lligada));
		novo->valor = l->valor;
		novo->prox = r;
		r = novo;
		l = l->prox;
	}
	return r;
}

//18
int maximo (LInt l){
    int max=0;
    while(l!=NULL){
        if(l->valor>max) max=l->valor;
        l=l->prox;
    }
    return max;
}
//19
int take (int n, LInt *l){
    int tam=0;
        while((*l)!=NULL && n>0){
         l=&((*l)->prox);
         n--;
         tam++;
        }
        if((*l)!=NULL){
            LInt tmp=(*l);
            free(tmp);
            (*l)=NULL;
        }
    return tam;
}
//20
int drop (int n, LInt *l) {
     int rem=0;
   while((*l)!=NULL && n>0){
       LInt tmp=(*l);
       (*l)=(*l)->prox;
       free(tmp);
       n--;
       rem++;
   } 
   return rem;
}
//21
LInt Nforward (LInt l, int N){
    while(l!=NULL && N>0){
        l=l->prox;
        N--;
    }
    return l;
}
//22
int listToArray (LInt l, int v[], int N){
    int p=0,i;
    for(i=0;i<N && l!=NULL;i++,l=l->prox){
        v[i]=l->valor;
        p++;
    }
    return p;
}
//23
LInt arrayToList (int v[], int N){
   int i;
    for(i=0;i<N;i++){
       LInt novo=malloc(sizeof(struct lligada));
        novo->valor=v[i];
        novo->prox=arrayToList(v+1,N-1);
    return novo;
    }
}
//24
LInt somasAcL (LInt l) {
 	LInt aux, *r = &aux;
	int ac = 0;
	while(l!=NULL) {
		ac += l->valor;
		*r = malloc(sizeof(struct lligada));
		(*r)->valor = ac;
		l=l->prox;
		r=&((*r)->prox);
	}
	*r = NULL;
	return aux;
}

//25
void remreps (LInt l){
    LInt p;
    while(l!=NULL){
        p=l->prox;
        while( p!=NULL && l->valor==p->valor){
            LInt tmp=p->prox;
            free(p);
            p=tmp;
        }
        l->prox=p;
        l=l->prox;
    }
}
//26
LInt rotateL (LInt l){
    if(l==NULL || l->prox==NULL) return l;
    else{
    LInt tmp=l;
    LInt novo=l->prox;
    while(tmp->prox!=NULL) tmp=tmp->prox;
    tmp->prox=l;
    l->prox=NULL;
    return novo;
    }
}
//27 l=> pos impares 
LInt parte (LInt l){
  LInt aux, p = NULL;

	if(l!=NULL && l->prox!=NULL) {
		p = l->prox;
		while(l->prox != NULL) {
			aux = l->prox;
			l->prox = l->prox->prox;
			l = aux;
		}
	}
	return p;
}

//28
int altura (ABin a){
   int e,d;
   if(a==NULL) return 0;
   e=1+altura(a->esq);
   d=1+altura(a->dir);
   if(e>d) return e;
   else return d;
}
//29
ABin cloneAB (ABin a){
 ABin novo=NULL;
      if(a!=NULL){
        novo=malloc(sizeof(struct nodo));
        novo->valor=a->valor;
        novo->esq=cloneAB(a->esq);
        novo->dir=cloneAB(a->dir);
    }
    return novo;
}
//30
void mirror (ABin *a) {
     ABin b;
    if((*a)!=NULL){
        b=(*a)->dir;
        (*a)->dir=(*a)->esq;
        (*a)->esq=b;
        mirror(&((*a)->dir));
        mirror((&(*a)->esq));
}
}
//31 E R D
void inorderA(ABin a ,LInt *l){
    if(a!=NULL){
        inorderA(a->dir,l);
        LInt novo=malloc(sizeof(struct lligada));
        novo->valor=a->valor;
        novo->prox=(*l);
        (*l)=novo;
        inorderA(a->esq,l);
    }
}
void inorder (ABin a, LInt *l){
*l=NULL;
inorderA(a,l);
}
//32 R E D
void preorderA (ABin a , LInt *l){
    if(a!=NULL){
        preorderA(a->dir,l);
        preorderA(a->esq,l);
        LInt novo=malloc(sizeof(struct lligada));
        novo->valor=a->valor;
        novo->prox=(*l);
        (*l)=novo;
    }
}
void preorder (ABin a , LInt *l){
    *l=NULL;
    preorderA(a,l);
}
//33 E D R
void posorderA(ABin a ,LInt *l){
    if(a!=NULL){
        LInt novo=malloc(sizeof(struct lligada));
        novo->valor=a->valor;
        novo->prox=(*l);
        (*l)=novo;
        posorderA(a->dir,l);
        posorderA(a->esq,l);
    }
}
void posorder (ABin a, LInt *l){
 *l=NULL;
 posorderA(a,l);
}
//34
int depth (ABin a, int x){
   if(!a) return -1;

    if(a->valor == x) return 1;

    int esq = depth(a->esq,x);
    int dir = depth(a->dir,x);

    if(esq == -1 && dir == -1) return -1;
    if(esq == -1) return 1 + dir;
    if(dir == -1) return 1 + esq;
    return esq < dir ? 1 + esq : 1 + dir;
}
//35
int freeAB (ABin a){
       int n=0;
    if(a!=NULL){
     n = 1 + freeAB(a->esq) + freeAB(a->dir);
     free(a);   
    }
    return n;
     
}
//36
 int pruneAB (ABin *a, int l){
     int rem=0;
     if(a!=NULL){
        rem = pruneAB (&((*a)->esq),l-1) + pruneAB (&((*a)->dir),l-1);
     }
 }
//37
int iguaisAB (ABin a, ABin b){
    int ie,id;
    if(a!=NULL && b!=NULL){
        ie=iguaisAB(a->esq,b->esq);
        id=iguaisAB(a->dir,b->dir);
        if(a->valor== b->valor && ie && id) return 1;
    }
    if(a==NULL && b==NULL) return 1;
    return 0;
}
//38
//39
//40 E-R-D
int dumpAbin (ABin a, int v[], int N){
   int e=0;
   if(a!=NULL && N){
       e=dumpAbin(a->esq,v,N);
       if(e<N){
           v[e]=a->valor;
               return 1+e+ dumpAbin(a->dir,v+e+1,N-e-1);

       }
    else return N;
   }
   else return 0;
}
//41
//42
int contaFolhas (ABin a) {
        int f=0;
    if(a!=NULL){
      if(a->esq==NULL && a->dir==NULL) f++;
      else{
          f+= contaFolhas(a->esq);
          f+= contaFolhas(a->dir);
      } 
    }
    return f;
}

//43
//44
//45 testa se um inteiro pertence a uma abin de proc
int lookupAB (ABin a, int x){
    while(a!=NULL && a->valor!=x){
        if(a->valor>x) a=a->esq;
        else a=a->dir;
    }
    if(a!=NULL) return 1;
    else return 0;

}
//46 calcula o nivel a que um elemento esta numa arvore bin de procura
//e<r<d

int depthOrd (ABin a, int x) {
     int prof=1;
     while(a!=NULL){
         if(a->valor==x) return prof;
         else if(a->valor>x) a=a->esq;
         else a=a->dir;
         prof++;
     }
     return -1;
}
//47
//48
//49
//50 constroi uma Abin a partir de uma lista
void insereOrd(ABin p,ABin *a){
  while((*a)!=NULL){
      if((*a)->valor>p->valor)
      a=&((*a)->esq);
      else if((*a)->valor<p->valor)
       a=&((*a)->dir);
  }
  *a=p;
}

void listToBTree (LInt l, ABin *a){
     while(l!=NULL){
         ABin novo=malloc(sizeof(struct nodo));
         novo->valor=l->valor;
         novo->esq=NULL;
         novo->dir=NULL;
         insereOrd(novo,a);
         l=l->prox;
     }
}


//51


LInt getLInt(int len) {
    if(len == 0) return NULL;
    LInt new = malloc(sizeof(struct lligada));
    printf("Insere um valor: ");
    int num;
    scanf("%d",&num);
    new->valor = num;
    new->prox = getLInt(len - 1);
    return new;
}

int main(){
 LInt a;
 int len;
 printf("Comprimento da lista: ");
            scanf("%d",&len);
    a=getLInt(len);
       int b=biggestL(a);
       printf(" o maior elemento é: %d\n",b);
    return 0;
}