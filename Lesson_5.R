f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*-1} #need to multiply by (-1) in order to make it a max function
x = c(0,0)
optim(x,f)#par gives the max values

#Chapter 2, Problem 3
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                  6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
x = c(50000,50000)
ans = optim(x,Rev,method = "L-BFGS-B")
print(ans$par) #values of x[1] and x[2]
print(-ans$val) #revenue
Blue(ans$par) #how many blue to harvest
Fin(ans$par) #how mnay fin to harvest

#sensitivity analysis on r2 
R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                       6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans = optim(x,fr2,method = "L-BFGS-B")
  return(ans)
}
R2(.08)

ans.x1 = 0
ans.x2 = 0
ans.rev = 0
r = seq(.04,.12,.01)
for (i in 1 :length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = ans$value
}
result = data.frame(growth_rate = r, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)
