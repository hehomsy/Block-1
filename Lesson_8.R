#Number 1 part b board sheet
library(MASS)
library(NlcOptim)
obj = function(x){
  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+.5*(x[3]-32)*x[1]^2)*(-1))
}
x = c(5.49,1.33,42.67)#solution for part a 
con = function(x){
  f = NULL
  f = rbind(f, x[3]^2*x[1]-10000)
  f = rbind(f, -32*x[2]+(x[3]-32)*x[1])
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun=con)


fun = function(x){
  return(((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1))
}
x = c(200,200)
con = function(x){
  f = NULL
  f = rbind(f,(x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun=fun,confun=con)
