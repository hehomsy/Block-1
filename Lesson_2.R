fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

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

##Solve this Problem
profit = function(t){(.65-.01*t)*(200+5*t)-.45*t}
t = 0:20
plot(t,profit(t),type="l")
dProfit = function(t){fprime(profit,t)}
ans = bisection(dProfit,0,20)
print(ans)
print(profit(ans))

##What would happen if price changed (sensitivity)
ans.time = 0
ans.prof = 0
r = seq(.008,.12,.001)
for (i in 1:length(r)){
  profit = function(t){(.65-r[i]*t)*(200+5*t)-.45*t}
  dProfit = function(t){fprime(profit,t)}
  ans.time [i] = bisection(dProfit(profit,0,20))
  ans.prof[i] = profit(ans.time[i])
  }
result = data.frame(price =r, time = ans.time, profit = ans.prof)
print (result)
