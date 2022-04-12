#include <stdio.h>

//Exercicio 1

/* a)
int main () {
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
 
 1 1 4 
 2 2 6
 3 3 8
 4 4 10
 5 5 12

 R:5 5 12

 b)
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
R:13
*/

//Exercicio 2 troca o valor de duas variaveis 
void swapM(int *x,int *y){
    int tmp=*y;
    *y=*x;
    *x=tmp;
}

//Exercicio 3 troca o valor das posições i e j 
void swap(int v[],int i,int j){
     int tmp=v[i];
     v[i]=v[j];
     v[j]=tmp;
}

//ex4 soma todos os elementos de um array
int  soma(int v[],int N){
     int i,sum=0;
     for(i=0;i<N;i++) sum+=v[i];
     return sum;
}

//exercicio 5  inverter o array utilizando uma das funções anteriores
void inverteArray(int v[],int N){
    int i;
     for(i=0;i<N/2;i++){
         swap(v,i,N-i-1);
     } 
}

//alternativa
void inverteArray2(int v[],int N){
int i,j=N-1;
     for(i=0;i<j;i++){
         swap(v,i,j);
         j--;
     } 
}

//exercicio 6 coloca em *m o maior elemento
int maximum(int v[],int N,int *m){
    int i,maior=v[0];
    for(i=1;i<N;i++){
        if(v[i]>maior){
            maior=v[i];
        }
        *m=maior;
    }
    return maior;
}

//exercicio 7
void quadrados (int q[], int N){
    int i;
    for(i=0;i<N;i++) q[i]=(i+1)*(i+1);
}

//exercicio 8
//a)
void pascal (int v[],int N){
    int i;
    if(N == 1) v[0] = 1;
    else {
        int prevLine[N - 1];
        pascal(prevLine,N-1);
        v[0] = 1;
        v[N - 1] = 1;
        for(i = 1; i < N - 1; i++) {
            v[i] = prevLine[i - 1] + prevLine[i];
        }
    }
}
//b)

void desenhaTriangulpP (int N){
    int i,u;
 for (i = 1; i <= N;i++){
        if (i != N)printf("%*c",(N-i), ' ');
        int line[i];
        pascal (line,i);
        for ( u = 0; u < i; u++)printf ("%d ",line[u]);
        putchar ('\n');
    }
}



void dumpV(int v[],int N){
    int i;
    for(i=0;i<N;i++) printf("%d ",v[i]);
    putchar('\n');
}



int main(){
    int arr[7]={2,5,13,15,18,9,6};
    //ex2
    int x=3,y=5;
    swapM(&x,&y);
    printf("x= %d y= %d\n",x,y);
    //ex3 
    swap (arr,0,5);
    printf ("%d %d\n", arr[0], arr[5]);
    //ex4 
    int s=soma(arr,7);
    printf("A soma dos elementos é de %d\n",s);
    //ex5
    inverteArray(arr,7);
    printf("Array invertido  ");
    dumpV(arr,7);
    //ex6
    int m=maximum(arr,7,&y);
    printf("o maior elemento é %d\n",m);
    //ex7
    quadrados(arr,7);
    dumpV(arr,7);
    return 0;
}


