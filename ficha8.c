#include <stdio.h>
#include <stdlib.h>

typedef LInt Stack;

typedef struct {
LInt inicio,fim;
} Queue;

typedef struct slist {
int valor;
struct slist * prox;
} * LInt;

LInt newLInt (int x, LInt xs) {
    LInt r = malloc (sizeof(struct slist));
    if (r!=NULL) {
        r->valor = x; r->prox = xs;
}
return r;
}
typedef LInt QueueC;

//1 Stacks inserção e remoção no inicio da lista

//a 
void initStack(Stack *s){
    (*s)=NULL;
}
//b
int SisEmpty(Stack s){
    return(s==NULL);
}

//c
int push(Stack *s, int x){
  *s=newLInt(x,*s);
}

//d
int pop(Stack *s, int *x){
    if((*s)==NULL) return -1;
    *x=(*s)->valor;
    (*s)=(*s)->prox;
    return 0;
}
//e
int top(Stack s,int *x){
    if(s==NULL) return -1;
    *x=s->valor;
    return 0;
}

//2 Queues inserção no final e remoção no inicio da lista

//a
void initQueue (Queue *q){
q->inicio=NULL;
q->fim=NULL;
}

//b
int QisEmpty (Queue q){
    return(q.inicio==NULL);
}

//c
int enqueue (Queue *q, int x){
  LInt tmp=newLInt(x,NULL);
    if(q->inicio==NULL){
        q->inicio=tmp;
        q->fim=tmp;
    }
    else{
        q->fim->prox=tmp;
        q->fim->prox=tmp;
    }
}

//d
int dequeue (Queue *q, int *x){
    if(q->inicio==NULL) return -1;
    *x=q->inicio->valor;
    LInt tmp=q->inicio;
    q->inicio=q->inicio->prox;
    free(tmp);
    return 0;
}

//e
int front (Queue q, int *x){
    if(q.inicio==NULL) return -1;
    *x=q.inicio->valor;
    return 0;
}

// 3 QueueC lista circular

//a
void initQueueC (QueueC *q){
    *q=NULL;
}

//b
int QisEmptyC (QueueC q){
    return(q==NULL);
}

//c
int enqueueC (QueueC *q, int x){
    if (*q == NULL) {
        *q = newLInt(x, NULL);
        (*q)->prox = *q;
    } else {
        (*q)->prox = newLInt(x, (*q)->prox);
        (*q) = (*q)->prox;
    }
    return 0;
}

//d
int dequeueC (QueueC *q, int *x){
    if (*q == NULL) return -1;
    LInt temp = NULL;
    if (*q == (*q)->prox) {
        temp = *q;
        *q = NULL;
    } else {
        temp = (*q)->prox;
        (*q)->prox = temp->prox;
    }
    *x = temp->valor;
    free(temp);
    return 0;
}

//e
int frontC (QueueC q, int *x){
    if (q == NULL) return -1;
    *x = q->valor;
    return 0;
}

//4
typedef struct dlist {
    int valor;
    struct dlist *ant, *prox;
} *DList;

typedef struct {
    DList back, front;
} Deque;

//a
void initDeque (Deque *q){
    q->back = NULL;
    q->front = NULL;
}

//b
int DisEmpty (Deque q){
    return (q.front == NULL);
}

//c
int pushBack (Deque *q, int x){
    DList temp = newDList(x, NULL);
    temp->ant = q->back;
    if (q->back == NULL) q->front = temp;
    else q->back->prox = temp;
    q->back = temp;
    
    return 0;   
}

//d
int pushFront (Deque *q, int x){
     DList temp = newDList(x, q->front);
    if (q->front == NULL) q->back = temp;
    else q->front->ant = temp;
    q->front = temp;
    
    return 0;
}

//e
int popBack (Deque *q, int *x){
    if (q->back == NULL) return -1;
    
    DList temp = q->back;
    *x = temp->valor;
    q->back = temp->ant;
    free(temp);
    
    if (q->back != NULL) q->back->prox = NULL;
    else q->front = NULL;
    
    return 0;
}

//f
int popFront (Deque *q, int *x){
    if (q->front == NULL) return -1;
    
    DList temp = q->front;
    *x = temp->valor;
    q->front = temp->prox;
    free(temp);
    
    if (q->front != NULL) q->front->ant = NULL;
    else q->back = NULL;
    
    return 0;
}

//g
int popMax (Deque *q, int *x){
    DList max = q->front;
    if (max == NULL) return -1;
    DList temp = max;
    
    while (temp->prox != NULL) {
        if (temp->valor > max->valor) max = temp;
        temp = temp->prox;
    }
    
    *x = max->valor;
    DList anterior = max->ant;
    DList seguinte = max->prox;
    
    if (anterior == NULL) { // É primeiro elemento
        if (seguinte != NULL) { // É primeiro e último
            q->back = NULL;
            q->front = NULL;
        }
        q->front = seguinte;
        seguinte->ant = NULL;
    } else if (seguinte != NULL) { // É último elemento
        q->back = anterior;
        anterior->prox = NULL;
    }
    
    free(max);
    return 0;    
}

//h
int back (Deque q, int *x){
    if (q.back == NULL) return -1;
    *x = q.back->valor;
    return 0;
}

//i
int front (Deque q, int *x){
     if (q.front == NULL) return -1;
    *x = q.front->valor;
    return 0;
}