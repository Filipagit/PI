#include <stdio.h>

//1
//a conseguiii
/*int main () {
int x [15] = {1, 2, 3, 4, 5,
6, 7, 8, 9,10,
11,12,13,14,15};
int *y, *z, i;
y = x;
z = x+3;
for (i=0; i<5; i++) {
printf ("%d %d %d\n",
x[i], *y, *z);
y = y+1; z = z+2;
}
1. x[i]=1 y=1 z=4 i=0
2. x[i]=2 y=2 z=6 i=1
3. x[i]=3 y=3 z=8  i=2 //incrementa o i e dps é que imprime 
4. x[i]=4 y=4 z=10  i=3
5. x[i]=5 y=5 z=12  i=4
6. y=6 z=14  x[i]=6 i=5 logo ja nao entra no ciclo for 
//resp: 5 5 12 

//b 
int main () {
int i, j, *a, *b;
i=3; j=5;
a = b = 42;
a = &i; b = &j;
i++;
j = i + *b;
b = a;
j = j + *b;
printf ("%d\n", j);
return 0;
}

1.i=3 j=5  a=42 b=42
2.a=&3 b=&5 i=4
3.j=4+*(&5)=4+5=9
4.b=&4
5.j=9+4=13
resp:13
*/

//2 x=3 y=5 => x=5 y=3 conseguiiii
void swapM(int *x,int *y){
    int tmp=*x;
    *x=*y;
    *y=tmp;
}

//3 troca o valor das posicoes i e j  conseguuiii
void swap(int v[], int i, int j){
    int tmp;
    tmp=v[i];
    v[i]=v[j];
    v[j]=tmp;
}
/*
int main(){
  int a, b, v[10] = {10, 6, 2, 3, 5, 1, 5, 9, 8, 7};
    printf ("%d %d\n", v[0], v[9]);
    swap (v,0,9);
    printf ("%d %d\n", v[0], v[9]);
    return 0;
}
*/
//4 calcula a soma dos elementos de um vetor  conseguiiii
int soma(int v[],int N){
    int i;
    int sum=0;
    for(i=0;i<N;i++){
     sum+=v[i];
    }
    return sum;
}
/*
int main(){
    int v[10]={10,6,2,3,5,1,5,9,8,7};
    int x=soma(v,10);
    printf("%d\n",x);
}
*/

//5 versao 1 conseguiiii
void inverteArray(int v[], int N){
    int i,j=N-1;
    for(i=0;i<N && i<j;i++){
        swap(v,i,j);
        j--;
    }
} 

//5 versao2 conseguiiiiiiiii
void inverteArray2(int v[], int N){
    int i,j=N-1;
    for(i=0;i<N && i<j;i++){
        swapM(&i,&j);
        j--;
    }
} 
/*
void dumpV (int v[], int N){
    int i;
    for (i=0; i<N; i++) printf ("%d ", v[i]);
    putchar ('\n');
}

int main(){
    int v[10]={10,6,2,3,5,1,5,9,8,7};
    printf("array inicial ");dumpV (v,10);
    inverteArray(v,10); 
    printf("array invertido "); dumpV (v,10);

}
*/

//6 coloca em *m o maximo conseguii
int maximum(int v[],int N, int *m){
     int i,maior=0; //maior=v[0]
     for(i=0;i<N;i++){
         if(v[i]>maior){ 
          maior=v[i];
         }
     }
     *m=maior;
     return maior;
}

//7 preenche o vetor c os quadrados dos N primeiros nrs naturais conseguiii
void quadrados(int q[], int N){
   int i;
   for(i=0;i<N;i++){
       q[i]=(i+1)*(i+1);
   }
}

//8

//a preenche o array v c os elementos da linha N do tp
void pascal (int v[],int N){
 int i,j;
 for(i=0;i<N;i++){
     j=i;
     while(j>=0){
         if(j==i) v[j]=1;
         else if(j>1) v[j]+=v[j-1];
         j--;
     }
      dumpV(v,i);
 }
}


//b escreve no ecran as N primeiras linhas do TP  nao seiii ver aula 
void desenhaTriangulpP (int N){
  int ,vaux[N]
   pascal(vaux,N); //so chama uma vez o pascal 
}

