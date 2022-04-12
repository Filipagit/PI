//exercicio 1 
struct staticStack {
int sp;
int values [Max];
} STACK, *SStack;

//a) inicializa uma stack
void SinitStack (SStack s){
    s->sp=0;
}

//b) testa se uma stack é vazia
int SisEmpty (SStack s){
return (s->sp==0);    
}

//c) acrescenta x ao topo de s 
int Spush (SStack s, int x){
    if(s->sp++ !=Max){
     s->values[s->sp++]=x;
     return 0
    }
   else  return 1;
}

//alternativa
int Spush (SStack s, int x){
    if(s->sp==Max) return 1;
    else{
        s->values[s->sp]=x;
        s->sp++;
    }  
     return 0;
}

//d) remove da stack o elemento do topo 
int Spop (SStack s, int *x){
    if(SisEmpty(s)==True) return 1;
    s->sp--;
    *x=s->values[s->sp];
    return 0;
    }
}

//e) coloca em x o elemento que esta no topo
int Stop (SStack s, int *x){
    if(SisEmpty(s)==True) return 1;
    else{
    *x=s->values[s->sp-1];
    return 0;    
    }
}

//exercicio 2 

struct staticQueue {
int front;
int length;
int values [Max];
} QUEUE, *SQueue;

//a) inicializa uma queue
void SinitQueue (SQueue q) {
q->length =0;
}

//b)
int SisEmptyQ(SQueue q){
    return (q->length == 0);
}

//c)
int Senqueue(SQueue q, int x){
    if(q->front + q->length >= MAX) return 1;
    else{
        q->values[q->front + q->length++] = x;
        return 0;
    }
}

//d)
int Sdequeue(SQueue q, int* x){
    if(q->length == 0) return 1;
    else{
        *x = q->values[(q->front)++];
        return 0;
    }
}

//e)
int Sfront(SQueue q, int* x){
    if(q->length == 0) return 1;
    else{
        *x = q->values[q->front];
        return 0;
    }
}