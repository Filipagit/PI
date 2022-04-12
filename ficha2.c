#include<stdio.h>

//exercicio 1

float multInt1(int n,float m){
    float r=0;
    int i;
      for(i=0;i<n;i++){
        r+=m;
    }
    return r;
}

// exercicio 2
/*
   | n  | m
1   81   423  
2   40   846  =423*2
3   20   1692 =846*2
4   10   3384 = 1692*2
5   5    6768 = 3384*2
6   2    13536 = 6768*2
7   1    27072 = 13536*2

se somar os valores de m para os quais n é impar fica:
423+6768+27072=34263 = 81*423

*/
float multInt2(int n,float m){
   float acm=0;
   while(n>0){
       if(n%2==1) acm+=m;
       n/=2;
       m=m*2;
   }
   return acm;
}

//exercicio 3  procura entre os divisores do menor entre a e b o maior que tmbm é divisor do outro
int mdc1(int a,int b){
  int tmp,ans;
    if(a<b){
     tmp=a;
     a=b;
     b=tmp;
    } //ver o maior comum
    for(int i = 1; i <= b; i++) {
        if(a % i == 0 && b % i == 0) ans = i;
    }
    return ans;

}

//exercicio 4 
int mdc2(int a,int b){
    while(a!=0 && b!=0){
     if(a>b) a=a-b;
     else if(a<b) b=b-a;
          else return a;
    }
    if(a!=0) return a ;
    else return b;
}

//exercicio 5 
int mdc3(int a,int b){
    while(a!=0 && b!=0){
     if(a>b) a%=b;
     else b%=a;
    }
    if(a!=0) return a ;
    else return b;
}

//exercicio 6 
//a mt lenta
int fib1(int n){
    if(n<2) return 1;
    else return fib1(n-1) + fib1(n-2);
}

//b
int fib2(int n){
    int i=0,a=0,b=1,t;
   while(i<n){
     t=b;
     b=b+a;
     a=t;
     i++;
   }
   return b;
}

int main(){
    //ex1
    int a;
    float f,f1,f2;
   // printf("Introduza 2 numeros para a multiplicação:");
   // scanf("%d",&a); scanf("%f",&f);
   //f1=multInt1(a,f);
    //printf("o resultado da multiplicação é:%f\n",f1);
    //ex2 
    printf("Introduza 2 numeros para a multiplicação:");
    scanf("%d",&a); scanf("%f",&f);
    f2=multInt2(a,f);
    printf("o resultado da multiplicação é:%f\n",f2);
    return 0;
}

 
 