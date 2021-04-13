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

//3
void ordenaPorNum (Aluno t [], int N){
    int i;
    int menor=t[0].numero;
    for(i=1;i<N;i++){
        if(menor>t[i].numero){
            swap(t[i],menor);
        }
    }
}


