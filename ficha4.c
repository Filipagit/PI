#include <stdio.h>
#include <string.h>
//1 Funções sobre strings 
//1
int contaVogais(char *s){
    int conta=0,i;
    for(i=0;i<strlen(s);i++){
        if(s[i]=='a' || s[i]=='e' || s[i]=='i'|| s[i]=='o' || s[i]=='u' ||s[i]=='A' || s[i]=='E' || s[i]=='I'|| s[i]=='O' || s[i]=='U') conta++;
    }
    return conta;
}

//2 
// sem array auxiliar 
void removeI(char s[],int n){
    int j;
    for(j=n;s[j]!='\n';j++) s[j] = s[j+1];
}

int retiraVogaisRep(char *s){
   int rem=0,i=1;
   while(s[i]!='\0'){
     if(s[i]==s[i-1] && (s[i]=='a' || s[i]=='e' || s[i]=='i'|| s[i]=='o' || s[i]=='u' ||s[i]=='A' || s[i]=='E' || s[i]=='I'|| s[i]=='O' || s[i]=='U')){
      removeI(s,i);
      rem++;
     }
     else i++;
   }
   return rem;
}

//com array auxiliar 
int isVogal (char c){
   if(c=='a' || c=='e' || c=='i'|| c=='o' || c=='u' || c=='A' || c=='E' || c=='I'|| c=='O' || c=='U') return 1;
    return 0;
}

int retiraVogaisRep2(char *s){
    int i,u;
 char new[strlen(s)], ant = s[0];
    int count = 0; 
    new[0] = s[0];
    for ( i = 1, u = 1; s[i];i++){
        if (!(isVogal (s[i]) && isVogal(ant))){
            new[u++] = s[i];
            ant = s[i];
        }
        else count++;
    }
    strcpy (s,new);
    return count;
}

//3
int duplicaVogais(char *s){
    int i,u;
  char new[strlen(s)*2];
    int count = 0; 
    for ( i = 0, u = 0; s[i];i++){
        if (isVogal(s[i])){
            new[u++] = s[i];
            new[u++] = s[i];
            count++;
        }
       else new[u++]=s[i];
    }
    strcpy (s,new);
    return count;
}

//2 Arrays Ordenados

//1 testa se um array esta ordenado por ordem crescente
int ordenado(int v[],int N){
    int i;
    for(i=0;i<N;i++){
        if(v[i]>v[i+1]) return 0;
    }
    return 1;
}
//2
void merge (int a[], int na, int b[], int nb, int r[]){
    int ia=0,ib=0,i=0;
    while(ia!=na && ib!=nb){
        if(a[ia]<=b[ib]){
            r[i]=a[ia];
            ia++;
        }
        else{
            r[i]=b[ib];
            ib++;
        }
        i++;
    }
    if(ia==na){
        while(ib!=nb){
            r[i]=b[ib];
            ib++;
            i++;
        }
    }else{
        while(ia!=na){
            r[i]=b[ia];
            ia++;
            i++;
    }    
}
}
void dumpV (int v[], int N){
    int i;
    putchar ('{');
    for (i=0; i<N; i++) printf ("%2d ", v[i]);
    putchar ('}');
}
//3

void swap(int v[], int i, int j){
    int tmp;
    tmp=v[i];
    v[i]=v[j];
    v[j]=tmp;
}

int partition(int v[],int N, int x){
   int i,j,k;
   int menores=0;
   for(k=0;k<N;k++){
       if(v[k]<=x) menores++;
   }
   for(i=0;i<menores;i++){
       if(v[i]>x){
           for(j=menores;j<N;j++){
               if(v[j]<=x) swap(v,i,j);
           }
       }
   }
   return menores;
}
int main(){
    int x;
    int a[5]={2,3,4,7,12};
    int b[5]={5,6,8,9,15};
    int d[10];
    char s1[100]="Estaa e umaa string coom duuuplicadoos";
    //ex1
    //printf("A string tem %d vogais\n",contaVogais(s1));
    //ex2
    //printf("foram removidas %d vogais.\n frase resultado:%s\n",retiraVogaisRep(s1),s1);
    //ex3
    //x = duplicaVogais (s1);
    //printf ("Foram acrescentadas %d vogais, resultando em \"%s\"\n", x, s1);
    //ex1
    //printf("%d\n",ordenado(a,5));
    //ex2
    //merge(a,5,b,5,d);dumpV(d,10);
    //ex3
    int v[9]={4,5,2,16,9,7,3,8,6};
    x=partition(v,9,6);dumpV(v,10);
    printf("o nr de elementos da 1parte é %d\n",x);
    return 0;
}
