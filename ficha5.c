#include<math.h>
#include<stdio.h>
#include<stdlib.h>

typedef struct aluno {
int numero;
char nome[100];
int miniT [6];
float teste;
} Aluno;

//1
int nota (Aluno a){
    int i,mt;
    float r;
    for(i=mt=0;i<6;i++)
     mt=a.miniT[i];
     r=(mt*0.25*20)/12+a.teste*0.75;
     if(r>=9.5) return round(r);
     else return 0;
}

//2
int procuraNum (int num, Aluno t[], int N){
    int i,r=-1;
    for(i=0;i<N && r==-1 && t[i].numero<=num;i++){
        if(num==t[i].numero) r=i;    
    }
    return r;
}

//3
void swapA(Aluno t[],int i,int j){
    Aluno tmp=t[i];
    t[i]=t[j];
    t[j]=tmp;
}

void ordenaPorNum (Aluno t [], int N){
    int i,j,m; 
    for(i=0;i<N-1;i++){m=i;
    for(j=i+1;j<N;j++)
        if(t[j].numero<t[m].numero) m=j;
          if(m!=i) swapA(t,i,m);
    }
}

//4
void criaIndPorNum(Aluno t [], int N, int ind[]){
    int i,j,m;
    for(i=0;i<N;i++) ind[i]=i;
    for(i=0;i<N-1;i++){
        m=i;
        for(j=i+1;j<N;j++){
            if(t[ind[i]].numero<t[ind[m]].numero)
            m=j;
            if(m!=i) swapA(ind,i,m);
        }
    }
}
//5
void imprimeTurma (int ind[], Aluno t[], int N){
    int i;
    for(i=0;i<N;i++)
    printf("%d %s %d\n",t[ind[i]].numero,t[ind[i]].nome,nota(t[ind[i]])); 
}

//6
int procuraNumalt (int num,int ind[], Aluno t[], int N){
 return procuraNum(num,t,N);
}


//7
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
void imprimeAluno (Aluno *a){
    int i;
    printf ("%-5d %s (%d", a->numero, a->nome, a->miniT[0]);
    for(i=1; i<6; i++) printf (", %d", a->miniT[i]);
    printf (") %5.2f %d\n", a->teste, nota(*a));
}
int main(){
    Aluno Turma1 [7] = {{4444, "André", {2,1,0,2,2,2}, 10.5}
                       ,{3333, "Paulo", {0,0,2,2,2,1},  8.7}
                       ,{8888, "Carla", {2,1,2,1,0,1}, 14.5}
                       ,{2222, "Joana", {2,0,2,1,0,2},  3.5}
                       ,{7777, "Maria", {2,2,2,2,2,1},  5.5}
                       ,{6666, "Bruna", {2,2,2,1,0,0}, 12.5}
                       ,{5555, "Diogo", {2,2,1,1,1,0},  8.5}
                       } ;
    int indNome [7], indNum [7];
    int i;
    
    printf ("\n-------------- Testes --------------\n");
    
    // ordenaPorNum (Turma1, 7);

    // printf ("procura 5555: %d \n", procuraNum (5555, Turma1, 7));
    // printf ("procura 9999:%d \n", procuraNum (9999, Turma1, 7));

    for (i=0; i<7; imprimeAluno (Turma1 + i++));
    
    // criaIndPorNum (Turma1, 7, indNum);
    
    // criaIndPorNome (Turma1, 7, indNome);

    // imprimeTurmaInd (indNum, Turma1, 7);
    // imprimeTurmaInd (indNome, Turma1, 7);

    // printf ("procura 5555:%d \n",  procuraNumInd (5555, indNum, Turma1, 7));
    // printf ("procura 9999:%d \n",  procuraNumInd (9999, indNum, Turma1, 7));

    printf ("\n---------- Fim dos Testes ----------\n");
    
    return 0;
}