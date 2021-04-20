#include<math.h>
#include<stdio.h>
#include<stdlib.h>

typedef struct aluno {
int numero;
char nome[100];
int miniT [6];
float teste;
} Aluno;

//1 calcula a nota de um aluno de acordo c pi conseguiii
int nota (Aluno a){
 int j;
    float summini=0;
            for(j=0;j<6;j++){
               summini+= a.miniT[j]*10;//para ficar de 0 a 20;
            } 
            if((summini/6)>=8 && a.teste>=8){
             float notaf=(summini/6)*0.3+a.teste*0.7;
             return round(notaf);
            }
       
            else return 0;
}

//2 conseguiiii
int procuraNum (int num, Aluno t[], int N){
    int i;
    for(i=0;i<N;i++){
        if(num==t[i].numero) return i;    
    }
    if(num!=t[i].numero) return -1;
}
/* int main() {
    Aluno Turma1 [7] = {{4444, "André", {2,1,0,2,2,2}, 10.5}
                       ,{6666, "Bruna", {2,2,2,1,0,0}, 12.5}
                       ,{8888, "Carla", {2,1,2,1,0,1}, 14.5}
                       ,{5555, "Diogo", {2,2,1,1,1,0},  8.5}
                       ,{2222, "Joana", {2,0,2,1,0,2},  3.5}
                       ,{7777, "Maria", {2,2,2,2,2,1},  5.5}
                       ,{3333, "Paulo", {0,0,2,2,2,1},  8.7}
                       } ;
  int r= procuraNum(5565,Turma1,7); 
  printf("%d\n",r);                
  return 0;
}   
*/

//3 ordena por ordem crescente do seu numero
void swap(int x, int y, Aluno t[]){
    Aluno tmp=t[x];
    t[x]=t[y];
    t[y]=tmp;
}

void ordenaPorNum (Aluno t [], int N){
    int i,j;
    int troca;//serve para verificar se houve troca ou nao 
    for(i=N-1;i>0;i--){
        troca=0;
    for(j=0;j<=i-1;j++){
        if(t[j].numero>t[j+1].numero){
          swap(j,j+1,t);
          troca=1;//ocorreu troca;
        }
    }
    if(!troca) break;//acabou
    }
}
//alternativa
void ordenaPorNum (Aluno t [], int N){
    int i,j;
    for(i=0;i<N-1;i++)
      for(j=i+1;j<N;j++)
      if(t[i].numero>t[j].numero) swap(t,i,j)
}
//4 preenche o array ind com os indices correspondentes a ordenacao do array 
void criaIndPorNum(Aluno t [], int N, int ind[]){
    Aluno tmp[N];
    for(int i=0;i<N;i++)
     tmp[i]=t[i];
     ordenaPorNum(tmp,N);
     for(int i=0;i<N;i++)
     ind[i]=procuraNum(t[i].numero,tmp,N);
}

//5
void imprimeTurma (int ind[], Aluno t[], int N){
    Aluno tmp[N];
    int i;
    for(i=0;i<N;i++){
      imprimeAluno(t+ind[i]);  
}

void imprimeAluno (Aluno *a){
    int i;
    printf ("%-5d %s (%d", a->numero, a->nome, a->miniT[0]);
    for(i=1; i<6; i++) printf (", %d", a->miniT[i]);
    printf (") %5.2f %d\n", a->teste, nota(*a));
}

//6
int procuraNumalt (int num,int ind[], Aluno t[], int N){
 return procuraNum(num,t,N);
}

//7 preenche ind c os indices correspondentes a ordenar por ordem crescente do nome do aluno 
//ordena por nome 
void ordenaPorNome(Aluno t[],int N){
    int i,j;
    for(i=0;i<N-1;i++)
      for(j=0;j<N-1;j++)
        if(strcmp(t[j].nome,t[j+1].nome)>0) swap(j,j+1,t);
}
int procuraNome (int nome, Aluno t[], int N){
    int i;
    for(i=0;i<N;i++){
        if(nome==t[i].nome) return i;    
    }
    if(nome!=t[i].nome) return -1;
}
void criaIndPorNome (Aluno t [], int N, int ind[]){
  Aluno tmp[N];
    for(int i=0;i<N;i++)
     tmp[i]=t[i];
     ordenaPorNome(tmp,N);
     for(int i=0;i<N;i++)
     ind[i]=procuraNome(t[i].nome,tmp,N);
}


