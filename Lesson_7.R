library(MASS)
library(NlcOptim)
solnl(X = NULL, objfun = NULL, confun = NULL, A = NULL, B = NULL,
      Aeq = NULL, Beq = NULL, lb = NULL, ub = NULL, tolX = 1e-05,
      tolFun = 1e-06, tolCon = 1e-06, maxnFun = 1e+07, maxIter = 4000)
## Example 2.2 - Meerschaert ##
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5000,8000,10000,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)

ans=solnl(x0,objfun=P,A=A,B=B)
print(ans$lambda)

## Same outer product as previous lesson ##
Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

X = list(x=seq(0,6000,100),y=seq(0,9000,100))
Z = Outer(P,X)
## This feasible region is an easy one to draw ###
contour(x=X$x,y=X$y,z=-Z,lwd=3)
abline(h=8000,col="red",lwd=3) #line at y=8000
abline(v=5000,col="red",lwd=3) #line at x=5000
abline(h=0,col="red",lwd=3) #line at y=0
abline(v=0,col="red",lwd=3) #line at x=0
abline(a=10000,b=-1,col="red",lwd=3) #line with y=bx+a; b is the slope, a is the y-intercept
pts = list(x=c(0,0,2000,5000,5000),y=c(0,8000,8000,5000,0)) #we need the intersection points for shading
polygon(pts$x,pts$y,density = 20,col="red") #this shades the feasible region red

## Example 2.3 - Meerschart ##
obj=function(x){
  return(-(x[1]+2*x[2]+3*x[3]))
}
#constraint function#
con=function(x){
  f=NULL
  f=rbind(f,x[1]^2+x[2]^2+x[3]^2-3)
  return(list(ceq=f,c=NULL))
}
x0=c(1,1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans)

## Example 2.4 - Meerschaert ####
obj=function(x){
  return(-(x[1]+2*x[2]+3*x[3]))
}
#constraint function
con=function(x){
  f=NULL
  f=rbind(f,x[1]^2+x[2]^2+x[3]^2-3)
  f=rbind(f,x[1]-1)
  return(list(ceq=f,c=NULL))
}
x0=c(1,1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans$par)
print(-ans$fn)
print(ans$lambda)

obj=function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
#constraint function
con=function(x){
  f=NULL
  f=rbind(f,x[1]+x[2]-10000)
  return(list(ceq=f,c=NULL))
}
x0=c(1,1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans$par)
print(-ans$fn)
print(ans$lambda)

#Number 3
obj = function(x){(10*x[1]^0.6)*(x[2]^.04)*(-1)} #because max, multiply by -1

#Contour Plot
X = list(x=seq(0,20,.1),y = seq(0,20,.1))
Z = Outer(obj,X)
contour(x = X$x, y = X$y, z = -Z, lwd = 1)
abline(a = 10, b = -5/3, col = "red")

contour(x = X$x, y = X$y, z = -Z, lwd = 1, levels = c(20,37.55,60,80,100,120,140))
abline(a = 10, b = -5/3, col = "red")

#Solution
library(MASS); library(NlcOptim)
x0 = c(3,5)
Aeq = matrix(c(50,30),nrow = 1)
Beq = matrix(300)
ans = solnl(x0,obj,Aeq = Aeq, Beq = Beq)
print(ans)

#Number 1
obj = function(x){(1/2*(x[2]-32)*x[1]^2)*(-1)}
con = function(x){
  f = NULL
  f = rbind(f,x[2]^2*x[1]-10000)
  return(list(ceq=f,c=NULL))
}
x0 = c(1,100)
solnl(x0,objfun = obj,c = NULL)
