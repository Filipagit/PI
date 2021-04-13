#include <stdio.h>
//1 Estados e Atribuições 

/* 1 conseguiii
int x, y;
x = 3; y = x+1;
x = x*y; y = x + y;
printf("%d %d\n", x, y);

1. x=3 y=4 
2.x=12 y=16

/2 conseguiii
int x, y;
x = 0;
printf ("%d %d\n", x, y);
x=0, y nao tem valor nao foi inicializada 

/3 ’A’, ’0’, ’ ’ ,’a’ 
    65, 48    32   97
char a, b, c;
a = ’A’; b = ’ ’; c = ’0’;
printf ("%c %d\n", a, a);
a = a+1; c = c+2;
printf ("%c %d %c %d\n", a, a, c, c);
c = a + b;
printf ("%c %d\n", c, c);

1.a=65 b=32 c=48
2.A 65 
3.B 66 2 (c) 50
4. a 66 c 50
5. b 98

/4 conseguiii
int x, y;
x = 200; y = 100;
x = x+y; y = x-y; x = x-y;
printf ("%d %d\n", x, y);

1. x=200 y=100
2. x=300 y=200 
3.x=100 y=200

//2 Estruturas de controlo 

//1a conseguiii
int x, y;
x = 3; y = 5;
if (x > y)
y = 6;
printf ("%d %d\n", x, y);

1.x=3 y=5

//b conseguiii
int x, y;
x = y = 0;
while (x != 11) {
x = x+1; y += x;
}
printf ("%d %d\n", x, y);

1.x=0 y=0
2.x=1 y=1
3.x=2 y=3
4.x=3 y=6
5.x=4 y=10
6.x=5 y=15
7.x=6 y=21
8.x=7 y=28
9.x=8 y=36
10. x=9 y=45
11.x=10 y=55
12.x=11 y=66

//c conseguiii
int i;
for (i=0; (i<20) ; i++)
if (i%2 == 0) putchar (’_’);
else putchar (’#’);

_#_#_#_#_#_#_#_#_#_#

//d
void f (int n) {
while (n>0) {
if (n%2 == 0) putchar (’0’);
else putchar (’1’);
n = n/2;
}
putchar (’\n’);
}
int main () {
int i;
for (i=0;(i<16);i++)
f (i);
return 0;
}
1
     10
     11
     100
     101
     110
     111
     1000
     1001
     1010
     1011
     1100
     1101
     1110
     1111
*/     

//3 Programas iterativos 

//1 desenhar um quadrado de dimensão 5 conseguuii
/*
#####
#####
#####
#####
#####
*/

void quadrado(int n){
    int i,j;
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            putchar('#');
        }
        putchar('\n');
    }
}

//2 quaseee

/*
#_#_#
_#_#_
#_#_#
_#_#_
#_#_#
*/
void xadrez(int n){ 
    int i,j;
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){ 
        if((j+i)%2==0) putchar('#');
        else putchar('_');
    }
    putchar('\n');
}
}

//3 triangulo 5 
/*
# 
##
###
####
#####
####
###
##
#
*/

void triangulo1(int n){
 int i,j; 
 for(i=1;i<2*n;i++){
     for(j=1;j<2*n-i;j++){
        if(j<=i) putchar('#');
     }
     putchar('\n');
 }   
}

void triang2(int n) {
    for(int i = 1; i <= n; i++) {
        int j = i - 1;
        for(int k = 0; k < n - 1 - j; k++) putchar(' ');
        for(int k = 0; k < 1 + 2 * j; k++) putchar('#');
        putchar('\n');
    }
}



int main(int argc,int const *argv[]){
    quadrado(5);
    xadrez(5);
    triangulo1(5);
    triang2(5);
    return 0;
}


 