#include<stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

//1 imprime o maior elemento de uma seq 
int maior(){
    int nummaior=0;
    int lido=0;
    scanf("%d",&lido);
    nummaior=lido;
    while(lido!=0){
     scanf("%d",&lido);
    if(lido>nummaior) nummaior=lido;
    }
return nummaior;
}

//2 imprime  a media da seq
int media(){
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

//3 imprime o 2 maior elemento
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

//4 calcula o nr de bits a 1 no final da representação binaria de 1 numero
int bitsUm (unsigned int n){
    int numb=0;
    while(n!=0){
        if(n%2==1) numb++;
        n/=2;
    }
    return numb;
}

//5 calcula o nr de bits a 0 no final da representação binaria de um numero
int trailingZ (unsigned int n) {
    int numb=0;
    while(n!=0){
        if(n%2==0) numb++;
        n/=2;
    }
    return numb;
}

