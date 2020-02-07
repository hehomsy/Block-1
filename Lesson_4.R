library(ma391homsy)
newton = function(f,x0,eps=.0001){
  xn = x0
  n = 1
  while(abs(f(xn))>eps & n<1000){
    xn = xn-f(xn)/fprime(f,xn)
    n = n+1
  }
  return (xn)
}

f = function(x){x^2-4}

newton(f,3)
newton(f,-1)#other zero

#Problem 8

p = function(t){(0.65-.01*t)*((5*t-t^2/60)+200)-.45*t}
dP= function(t){fprime(p,t)}
ans = newton(dP,5)
print(ans)
print(p(ans))

#sensitivity for problem 8
months = seq(1,10)
ans.time = 0 #concerned about the time
ans.profit = 0 #concerned about the profit
for (i in 1:length(months)){
  m = months[i]
  p = function(t){(0.65-.01*t)*((5/m)*(m*t-t^2/60)+200)-.45*t}
  dP = function(t){fprime(p,t)}
  ans.time[i] = newton(dP,6) #6 is similar to the original answer, so we start there
  ans.profit[i] = p(ans.time[i])
}
result = data.frame(months=months,days=ans.time,profit=ans.profit)
print(result)
