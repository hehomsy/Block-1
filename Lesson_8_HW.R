##Problem 1
library(MASS)
library(NlcOptim)
obj = function(x){
  return(((600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1]))*(-1))
}
x = c(200,200)
con = function(x){
  f = NULL
  f = rbind(f, 12*x[1]+5*x[2]-25000)
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun=con)

##Problem 2
obj = function(x){
  return(((600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1]))*(-1))
}
A = matrix(c(12,5,-1,0,0,-1),nrow=3,byrow=T)
B = matrix(c(25000,0,0),nrow=3)
X = list(x=seq(0,500),y=seq(0,500))
ans = solnl(x0,objfun = obj,A=A,B=B)
print(ans)

Z = Outer(obj,X)
contour(X$x,X$y,Z)
abline(a=1000,b=-12/5,col="red",lwd=3)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)

## Problem 5
obj = function(x){
  return(((10+22*x[1]^(-.5)+1.3*x[2]^(-.1))
          -18*x[1]+x[2]*(5+15*x[2]^(-.5)+.8*x[1]^(-.08))
          -10*x[2]))*-1
}
x = c(10,10)
ans = optim(x,obj,method = "BFGS")
print(ans)

##Problem 6
obj = function(x){
  return(((10+22*x[1]^(-.5)+1.3*x[2]^(-.1))
          -18*x[1]+x[2]*(5+15*x[2]^(-.5)+.8*x[1]^(-.08))
          -10*x[2]))*-1
}
A = matrix(c(2,3,-1,0,0,-1),nrow = 3,byrow = T)
B = matrix(c(18,3,2),nrow = 3)
X = list(x=seq(0,10),y=seq(0,10))
x = c(10,10)
ans = solnl(x,objfun = obj,A=A,B=B)
print(ans)
Z = Outer(obj,X)
contour(X$x,X$y,Z)
abline(a=1000,b=-12/5,col="red",lwd=3)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)

##Problem 7
obj = function(x){
  return(((10+22*x[1]^(-.5)+1.3*x[2]^(-.1))
          -18*x[1]+x[2]*(5+15*x[2]^(-.5)+.8*x[1]^(-.08))
          -10*x[2]))*-1
}
A = matrix(c(-50,-100,1,0,0,1),nrow = 3,byrow = T)
B = matrix(c(600,7,5),nrow = 3)
X = list(x=seq(0,10),y=seq(0,10))
ans = solnl(x0,objfun = obj,A=A,B=B)
print(ans)
Z = Outer(obj,X)
contour(X$x,X$y,Z)
abline(a=1000,b=-12/5,col="red",lwd=3)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)

##Problem 8
obj = function(x){
  return(((10+22*x[1]^(-.5)+1.3*x[2]^(-.1))
          -18*x[1]+x[2]*(5+15*x[2]^(-.5)+.8*x[1]^(-.08))
          -10*x[2]))*-1
}
Aeq = matrix(c(2,3,1,0,0,1,0,0,1,0,0,1,0,0,1),nrow = 3,byrow=T)
Beq = matrix(c(18,3,2),nrow = 3)
A = matrix(c(0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1),nrow = 3,byrow = T)
B = matrix(c(0,0,0),nrow = 3)
X = list(x=seq(0,10),y=seq(0,10))
ans = solnl(x0,objfun = obj,A=A,B=B,Aeq=Aeq,Beq = Beq)
print(ans)
