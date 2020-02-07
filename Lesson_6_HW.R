library(MASS)
f = function(x){10000-(950-x[1])*(100*5000)-A}
A = function(x){(x[2]-50000)*(200*50000)}
x = c(100,200)
ans = optim(x,f,method = "BFGS")
print(ans$par) #values of x[1] and x[2]
print(-ans$val)
