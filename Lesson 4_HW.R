library(ma391homsy)
p = function(x){
  return(((339-.01*x[1]-.003*x[2])*x[1]
          +(399-.004*x[1]-.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
x = c(500,500)
ans = optim(x,p,method = "BFGS")
print(ans)


f = function(x){
  return(((.05*x[1])*(1-(x[1]/150000))-(10^(-8)*x[1]*x[2]))
         +((.08*x[2])*(1-(x[2]/400000))-(10^(-8)*x[1]*x[2])))
}
x = c(50000,250000)
ans = optim(x,f,method = "BFGS")
print(ans)
