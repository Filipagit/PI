#include<stdio.h>
#include<string.h>
//1 Funções sobre strings

//1 conta quantas vogais uma string tem 
int contaVogais(char *s){
    int tam=strlen(s);
    int i;
    int conta=0;
    for(i=0;i<tam;i++){
        if(s[i]=='a'||s[i]=='e'||s[i]=='i'||s[i]=='o'||s[i]=='u'||s[i]=='A'||s[i]=='E'||s[i]=='I'||s[i]=='O'||s[i]=='U') conta++;
    }
    return conta;
} 
/*
int main(){
   char  v[12]="hello world";
    int vog=contaVogais(v);
    printf("%d\n",vog);
    return 0;
}
*/

//2 remove de uma string todas as repetições consecutivas de vogais 
//sem arrayauxiliar
//
void remindice(char s[],int n){
    int j;
    for(j=n;s[j]!='\0';j++){
      s[j]=s[j+1];
    }
}
int retiraVogaisRep(char *s){
    int i=1;
    int rem=0;
    while(s[i]!='\0'){
        if ((s[i]==s[i-1]) && (s[i]=='a'||s[i]=='e'||s[i]=='i'||s[i]=='o'||s[i]=='u'||s[i]=='A'||s[i]=='E'||s[i]=='I'||s[i]=='O'||s[i]=='U')){
            remindice(s,i);
               rem++;
        }
        else i++;
    }
    return rem;
}

int main(){
    char *t="Estaa e umaa string coom duuuplicadoos";
    printf("vogais repetidas\nFrase inicial:%s\n",t);
    printf("foram removidas %d vogais.\nfrase resultante:%s\n",retiraVogaisRep(t),t);
    return 0;
}


/*
// com array auxiliar 
int retiraVogaisRep(char *s){
    int i,j=0;
    int repetidos=0;
    char aux[100]="";
    while(s[i]!='\0'){
        if(s[i]=='a'||s[i]=='e'||s[i]=='i'||s[i]=='o'||s[i]=='u'||s[i]=='A'||s[i]=='E'||s[i]=='I'||s[i]=='O'||s[i]=='U'){
              if(s[i]!=aux[j]){
                  j++;
                  aux[j]=s[i];
              }
              else repetidos++;
        } else{
             j++;
            aux[j]=s[i];
        }
        printf("\n%c - %d - %d\n",*s,j,repetidos);
        s++;
    }
    j++;
    aux[j]='\0';
    return repetidos;
}

int main(){
    char *t="Estaa e umaa string coom duuuplicadoos";
    printf("vogais repetidas\nFrase inicial:%s\n",t);
    printf("foram removidas %d vogais.\nfrase resultante:%s\n",retiraVogaisRep(t),t);
    return 0;
}
*/

//3
int duplicaVogais(char *s){
    int i,j,dup=0;
    int tam=strlen(s);
    for(i=0;s[i]!='\0';i++){
        if(s[i]=='a'||s[i]=='e'||s[i]=='i'||s[i]=='o'||s[i]=='u'||s[i]=='A'||s[i]=='E'||s[i]=='I'||s[i]=='O'||s[i]=='U'){
            for(j=tam;j>i;j--){
                s[j]=s[j-1];
            }
            dup++;
            tam++;
        }
    }
    return dup;
}


// 2 Arrays ordenados 

//1 testa se um array esta ordenado 
int ordenado(int v[], int N){
    int i;
    for(i=0;i<N-1;i++){
        if(v[i]>v[i+1]) return 0;
    }
    return 1;
}
/*
int main(){
    int v[5]={1,2,3,4,5};
    int s[5]={2,5,15,4,9};
    printf("%d\n",ordenado(v,5));
    printf("%d\n",ordenado(s,5));
}
*/

//2 recebe dois arrays ordenados a e b e os funde num so 
void merge (int a[], int na, int b[], int nb, int r[]){
    int ia,ib,i=0;
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
                i++;
                ib++;
            }
        }else{
            while(ia!=na){
            r[i]=a[ia];
                 i++;
                ia++;
        }
    }
}
/*
void dumpV (int v[], int N){
    int i;
    for (i=0; i<N; i++) printf ("%d ", v[i]);
    putchar ('\n');
}

int main(){
    int a[4]={1,3,5,7};
    int b[5]={2,4,6,8,10};
    int r[10]={};
    merge(a,4,b,5,r);dumpV(r,9);
}
*/

//3 reorganiza o array de modo a aparecer primeiro so menores que x 
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
/*
void dumpV (int v[], int N){
    int i;
    for (i=0; i<N; i++) printf ("%d ", v[i]);
    putchar ('\n');
}

int main(){
    int v[9]={4,5,2,16,9,7,3,8,6};
    partition(v,9,6);dumpV(v,10);
}
*/























