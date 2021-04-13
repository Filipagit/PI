#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>


//1 imprime o maior elemento da sequência conseguiii
int maior(){
    int nummaior=0;
    int lido;
    do{ 
    scanf("%d\n",&lido);
    while(lido!=0){
    if(lido>nummaior) nummaior=lido;
}
printf("%d\n",nummaior);
    }
return nummaior;
}

//2 conseguiii
void media(){
    int lido;
    int n=0;
    double sum=0;
    do{
        scanf("%d\n",&lido);
        sum+=lido;
        n++;
    }
    while(lido!=0);
    printf("%d\n",sum/n);
}

// 3 conseguiii
int segelemmaior(){
 int maior=0;
    int segmaior=0;
    int lido;
    do{
        scanf("%d\n",&lido);
        if(maior>lido && segmaior<lido)
        segmaior=lido;
        else if(maior<lido) 
        segmaior=maior;
        maior=lido;
    } 
    while(lido!=0);
        printf("%d\n",segmaior);
        return 0;
}

//4 conseguiii
int bitsUm (unsigned int n){
    int conta=0;
    while(n!=0){
        if(n%2==1) conta++;
        n/=2;
    }
    return conta;
}


//5 calcula o nr de bits a 0 no final conseguiii
int trailingZ(unsigned int n){
    int conta=0;
    while(n!=0){
       if(n%2==0) conta++;
       n/=2;
    }
    return conta;
}
/*
int main(){
    int y=100;
    int num=trailingZ(y);
    printf("resposta: %d\n",num);
    return 0;
}
*/ 

//6 conseguiii
int qDig(unsigned int n){
    int conta=0;
    while(n!=0){
        conta++;
        n/=10;

    }
    return conta;
}

//7 concatena s2 a s1 conseguiii
char *mystrcat(char s1[], char s2[]) {
   int i,j;
    for(i=0;s1[i]!='\0';i++);
    for(j=0;s2[j]!='\0';j++){
          s1[i+j]=s2[j];
    }
    s1[i+j]='\0';
    return s1;
}

//8 copia source para dest retornando dest conseguii
char *mystrcpy(char s1[], const char s2[]) {
    int i;
    for(i=0;s2[i]!='\0';i++){
        s1[i]=s2[i];
    }
    s1[i]='\0';
    return s1;
}

//9
int mystrcmp(char s1[], char s2[]){
    int i=0;
    while(s1[i]==s2[i] && s1[i]!='\0')  i++;
  return (s1[i] - s2[i]);
}

//10 determina a posicao onde s2(so uma palavra) ocorre em s1
char *strstr (char s1[], char s2[]){
int i,j;
for(i = 0, j = 0; s1[i] != '\0' && s2[j] != '\0'; i++)
    if(s1[i]==s2[j])
    j++;
    else 
          j = 0;

  if (s2[j] == '\0')
    return s1 += (i - j);
  else
    return NULL;
}

//11 inverte uma string conseguii
void strrev(char s[]){
     int i, tmp;
    int tam=strlen(s)-1;
    for(i=0;i<tam;i++){
      tmp=s[i];
      s[i]=s[tam];
      s[tam]=tmp;
      tam--;
    }
}

//12 retira todas as vogais de uma string conseguiii

void removeindice(char v[], int n){
    int j;
    for(j=n;v[j]!='\0';j++){
        v[j]=v[j+1];
    }
}

void strnoV(char s[]){
    int i=0;
    while(s[i]!='\0'){
        if(s[i]=='a' || s[i]=='e' || s[i]=='i' || s[i]=='o' || s[i]=='u' || s[i]=='A' || s[i]=='E' || s[i]=='I' || s[i]=='O' || s[i]=='U'){
            removeindice(s,i);
        }
        else i++;
    }
}

//13 quasee
// txt="liberdade, igualdade e fraternidade" truncW (txt, 4)="libe igua e frat".
void removeindice(char v[], int n){
    int j;
    for(j=n;v[j]!='\0';j++){
        v[j]=v[j+1];
    }
}

void truncW (char t[], int n){
   int i=0;
    int comppal=0;
    while(t[i]!='\0'){
        if(t[i]==' ' || t[i]=='\n' || t[i]=='\t'){
            i++;
            comppal=0;
        }
        else{
            if(comppal++ >=n) removeindice(t,i);
            else i++;
        }
    }
}

//14 retorna o caracter mais frequente conseguiii
int contachar(char s[],char a){
    int i;
    int conta=0;
    for(i=0;s[i]!='\0';i++){
        if(s[i]==a) conta++;
    }
    return conta;
}

char charMaisfreq(char s[]){
    int i;
    char maxf;
    int maisfreq=0;
    for(i=0;s[i]!='\0';i++){
       if(maisfreq<contachar(s,s[i])){
           maisfreq=contachar(s,s[i]);
           maxf=s[i];
       }
    }
    return maxf;
}

//15 conseguiii
int iguaisConsecutivos (char s[]) {
     int i;
    int consec=1;
    int maxconsec=0;
    for(i=0;s[i]!='\0';i++){
        if(s[i]==s[i+1]) consec++;
        else{
        if(maxconsec<consec) maxconsec=consec;
        consec=1;// para contar o nr de consecutivos dos restantes
    }
    }
    return maxconsec;
}

//16 nao ta a dar 
int difConsecutivos (char s[]){
    int dif=0;
    int maxdif=0;
    int i;
    for(i=0;s[i]!='\0';i++){
        if(s[i]!=s[i+1]) dif++;
        else{
            if(maxdif<dif) maxdif=dif;
            dif=1;
        }
    }
    return maxdif;
}
