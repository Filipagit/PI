//1
int nesimo(int a[],int N,int i){
    //ordenar a lista segundo a estrategia bubble sort
int j,k, m;
	for (j=0;j<N-1;j++){ m=j;
	for(k=j+1;k<N;k++)
	if(a[k]<a[m]) m=k;
		if (m!=j) swap (a,j,m);
	}

	return a[i-1];//i-1 pq i começa em 1
}

//2

typedef struct LInt_nodo {
int valor;
struct LInt_nodo *prox;
} *LInt;

LInt removeMaiores(LInt l, int x){
    LInt pt = l, ant = NULL;
	// fase 1: avançar
	while (pt!=NULL && pt->valor <= x){
		ant = pt; pt = pt->prox;
	}
	// fase 2: terminar a lista resultante
	if (ant == NULL) l = NULL;
	else ant->prox = NULL;
	// fase 3: libertar o resto da lista
	while (pt != NULL) {
		ant = pt; pt = pt->prox; free (ant);
	}
	return l;
}

//3

typedef struct ABin_nodo {
int valor;
struct ABin_nodo *esq, *dir;
} *ABin;

LInt caminho(ABin a, int x){
LInt r=NULL;
	if (a!=NULL){
		if (a->valor == x) r = newLInt (a->valor,NULL);
		else { 
			if (x<a->valor) r = caminho (a->esq,x);
			else r = caminho (a->dir,x);
			if (r != NULL) r = newLInt (a->valor,r);
		}
	}
	return r;
}

//4
void inc(char s[]){
    int tam = strlen(s);
    int i;

	for (i = tam-1; i>=0 && s[i] == '9'; s[i--]='0');
	// caso particular: a string aumenta de tamanho
	if (i==-1) { s[0] = '1'; s[l] = '0'; s[tam+1]='\0';}
    else s[i]++;
}

//5
int sacos(int p[], int N, int C){
    int i;
    int sum=0;
    int ns=0;
    for(i=0;i<N;i++) sum+=p[i];
    while(sum!=0){
        if(sum>C){
            sum-=C;
            ns++;
        }
    }
    return ns;
}

int ensacarAux (int p[], int N, int k, int i, int pesos[]){
    // admitindo que os i primeiros items já 
    // se encontram em sacos, tenta ensacar os restantes.
    // Significado do array pesos:
    //     pesos[j] tem a capacidade disponível do saco j
    int j;
    if (i==N) return 1;
    // tentar guardar o item i (com todas as alternativas) 
    // e depois (recursivamente) os restantes
    for (j=0; j= p[i];j++) {
    		// se o item i cabe no saco j
    		pesos[j] -= p[i];
    		// coloca-se no saco j e tenta-se o 
    		// resto dos items
    		if (ensacarAux_sol (p,N,k,i+1,pesos)) return 1;
    		// Se não houve solução, retira-se para 
    		// tentar outras alternativas
    		pesos[j] += p[i];
    	}
    // Se nada funcionou ...
    return 0;
}

int ensacar (int p[], int N, int C, int k) {
	// true se é possível ensacar com k sacos
	int pesos [k]; int i;
	for(i = 0; i < k; pesos[i++] = C)
		;
	// true sse é possível ensacar em k sacos
	return ensacarAux_sol (p,N,k,0,pesos);
}

int sacos (int p[], int N, int C){
	int i=0;
	while (! ensacar (p,N,C,i)) i++;
	return i;
}
