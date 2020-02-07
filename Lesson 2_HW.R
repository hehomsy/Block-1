## Define the function using boolean statements such as >, <=, == 
r = 5/7
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
x = seq(0.50)

bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}
C = function(x){fT(f,x)}
ans = bisection(C,0,50)
print(ans)
print(C(ans))
