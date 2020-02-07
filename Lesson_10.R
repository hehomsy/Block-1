## a = 0.01 ##
options(warn=-1)
library(MASS);library(NlcOptim);library(ma391homsy)
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
solnl(x0,objfun=P,A=A,B=B) #value will always be negative when maximizing, but write as positive
#only binding constraint is the third because $lambda$ineqlin is greater than zero

#if we increase third constraint by 1, we expect the function to change by 24
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5000,8000,10001,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)
#does in fact change by 24

#change each constraint by 1
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5001,8001,10001,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)
#still only changes by 24 because that is the only binding constraint, x1 and x2 values may change but objective function does not 

