// 1 Estado e Atribuicoes 

//1
int x, y;
x = 3;
y = x+1;
x = x*y; y = x + y;
printf("%d %d\n", x, y);
// x=3 y=4 ; x=12 y=16

//2 
int x, y;
x = 0;
printf ("%d %d\n", x, y);
// x=0 y nao se sabe 

//3
char a, b, c;
a = ’A’; b = ’ ’; c = ’0’;
printf ("%c %d\n", a, a);
a = a+1; c = c+2;
printf ("%c %d %c %d\n", a, a, c, c);
c = a + b;
printf ("%c %d\n", c, c);

// A 65 ; B 66 2 50 ; b 98

//4
int x, y;
x = 200; y = 100;
x = x+y; y = x-y; x = x-y;
printf ("%d %d\n", x, y);

//  x=300 y=200;x=100 y=200

//2 Estruturas de controlo

//a
int x, y;
x = 3; y = 5;
if (x > y)
y = 6;
printf ("%d %d\n", x, y);

// x=3 y=5

//b
int x, y;
x = y = 0;
while (x != 11) {
x = x+1; y += x;
}
printf ("%d %d\n", x, y);
// x=0 y=0; x=1 y=1; x=2 y=3; x=3 y=6 ; x=4 y=10; x=5 y=15 ; x=6 y=21 ; x=7 y=28;
//x=8 y= 36 ; x=9 y=45 ; x=10 y=55 ; x=11 y=66

//c
int i;
for (i=0; (i<20) ; i++)
if (i%2 == 0) putchar (’_’);
else putchar (’#’);
//_#_#_#_#_#_#_#_#_#_#

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
// 1;01;11;001 etc

// 3 Programas iterativos 

//1 Desenhar no ecra um quadrado de dimensao 5 

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

//2
/*
  01234
0 #_#_#
1 _#_#_
2 #_#_#
3 _#_#_
4 #_#_#
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
}

//3
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

/*
    #
   ###
  #####
 #######
#########

*/

void triangulo2(int n){
    for(int i = 1; i <= n; i++) {
        int j = i - 1;
        for(int k = 0; k < n - 1 - j; k++) putchar(' ');
        for(int k = 0; k < 1 + 2 * j; k++) putchar('#');
        putchar('\n');
    }
}