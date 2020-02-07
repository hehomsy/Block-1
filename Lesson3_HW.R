profit = function(t){(650+5*t)*(-.25-.0*t)-.45*t-140.5}
dT = function(t){fprime(profit,t)} #where derivative is equal to zero
plot(t,profit(t))
bisection(dT,0,150)

library(ma391homsy)

profit2 = function(t){(200-5*t)*(.65-.01*t)-.45*t}
dT = function(t){fprime(profit2,t)} #where derivative is equal to zero
plot(t,profit2(t))
bisection(dT,0,150)


#Newton's Method
f = function(x){x^2}
f1 = function(x){fprime(f,x)}
x = 2
tol = .00001
root = function(f,f1,x,tol){
  while (abs(f(x))>tol){
    x = x-(f(x)/f1(x))
  }
  x
}
root(f,f1,x,tol)
