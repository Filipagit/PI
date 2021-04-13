
//1 conseguiii
float multInt1(int n, float m){
    float r=0;
    int i;
    for(i=0;i<n;i++){
      r+=m;
    }
    return r;
}

//2 conseguiiii
float multInt2(int n,float m){
    float r=0;
    while(n>0){
        if(n%2==1)r+=m;
         n=n/2;
         m=m*2;
    }
    return r;
}


//3
int mdc1(int a, int b){
  int temp, ans;
    if(a < b) {
        temp = a;
        a = b;
        b = temp;
    }
    for(int i = 1; i <= b; i++) {
        if(a % i == 0 && b % i == 0) ans = i;
    }
    return ans;
}

//4 conseguiii
int mdc2(int a,int b){
    while(a!=0 && b!=0){
        if(a>b)a=a-b;
        else if(a<b) b=b-a;
        else return a;
    }
    if(a==0) return b;
    else return a;
}

//5 conseguii
int mdc3(int a,int b){
  while(a!=0 && b!=0){
      if(a>b) a%=b;
      else b%=a;
  }
  if(a==0) return b;
  else return a;
}

//6
//a conseguii
int fib1(int n){
    if(n==1 ||n==2) return 1;
    else return fib(n-1) + fib(n-2);
}

//b
int fib2(int n){
    int ult=1,antult=1;
    int i,temp;
    for(i=3;i<=n;i++){
        temp=antult;
        antult+=ult;
        ult=temp;
    }
    return antult;
    
}
