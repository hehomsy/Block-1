library(ma391homsy)
profit = function(x){((339-.01*x[1]-.003*x[2])*x[1]+(399-.004*x[1]-.01*x[2])*x[2])-((400000+220*x[1]+250*x[2]))*-1}
x = c(500,500)
ans = optim(x,profit,method = "BFGS")
print(ans$par) #values of x[1] and x[2]
print(-ans$val) #profit
 