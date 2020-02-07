##Problem 1a
obj = function(x){
  return(x[1]^2+x[2]^2)
}
Aeq = matrix(c(1,2),nrow = 1, byrow = T)
Beq = matrix(c(5),nrow = 1)
A = matrix(c(-1,0,0,-1),nrow = 2,byrow = T)
B = matrix(c(0,0), nrow = 1)
ans = solnl(x,objfun = obj,A=A,B=B,Aeq = Aeq,Beq = Beq)
print(ans)

##Problem 1b
obj = function(x){
  return((x[1]^2-x[2]^2)*-1)
}
con = function(x){
  f = NULL
  f = rbind(f, 2*x[2]-x[1]^2)
  return(list(ceq=f,c=NULL))
}
x = c(1,1)
ans = solnl(x,objfun = obj,confun=con)
print(ans)


##Problem 1c
obj = function(x){(exp(-x[1]*x[2]/4))}
con = function(x){
  f = NULL
  f = rbind(f,x[1]^2+x[2]^2-1)
  return(list(ceq=f,c=NULL))
}
x = c(1,1)
ans = solnl(x,objfun = obj,confun=con)
print(ans)

obj = function(x){(exp(-x[1]*x[2]/4))*-1}
con = function(x){
  f = NULL
  f = rbind(f,x[1]^2+x[2]^2-1)
  return(list(ceq=f,c=NULL))
}
x = c(1,1)
ans = solnl(x,objfun = obj,confun=con)
print(ans)

##Problem 1d
obj = function(x){(x[1]^2+x[2]^2+x[3]^2)*(-1)}
Aeq = matrix(c(1,0,2,1,1,0),nrow = 2,byrow = T)
Beq = matrix(c(6,12),nrow = 2)
A = matrix(c(-1,0,0,0,-1,0,0,0,-1),nrow = 3,byrow = T)
B = matrix(c(0,0,0),nrow = 3)
x = c(0,10,10)
ans = solnl(x,objfun = obj,A=A,B=B,Aeq=Aeq,Beq=Beq)
print(ans)

##Problem 2 