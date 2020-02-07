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

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
X = list(x=seq(3000,6000,100),y=seq(6000,8000,100))
Z = Outer(P,X)
contour(x=X$x,y=X$y,z=-Z)
persp(x=X$x,y=X$y,Z,theta=40,ticktype = "detailed",shade=0.01)

Product = function(f,X){
  #X is a list with two vector of numbers, f is a function that returns the optimal f
  #output is a table of x,y,f(x,y) values and a matrix of x,y,f(x,y) values
  col1=0
  col2=0
  l = length(X[[1]])*length(X[[2]])
  fun=data.frame(x=1:l,y=1:l,f=1:l)
  n = 0
  z = matrix(0,nrow=length(X[[1]]),ncol=length(X[[2]]))
  for (i in 1:length(X[[1]])){
    for (j in 1:length(X[[2]])){
      n=n+1
      fVal = f(c(X[[1]][i],X[[2]][j]))
      fun$x[n]=X[[1]][i]
      fun$y[n]=X[[2]][j]
      fun$f[n]=fVal
      z[i,j]=fVal #matrix of z values for all (x row, y col)
    }
  }
  #df is a table of x*y rows of function values, fun is a matrix of x rows and y cols
  ans = list(df=fun,z=z)
  return(ans)
}
Z=Product(P,X)
library(ggplot2)
library(RColorBrewer)
df = Z$df
v = ggplot(df, aes(x, y, z = f))
v=v + geom_contour(aes(colour = stat(level)))
v
v=ggplot(df, aes(x, y, z = f))+
  geom_tile(aes(fill=f))+
  stat_contour(bins=6,aes(x,y,z=f), color="black", size=0.6)+
  scale_fill_gradientn(colours=brewer.pal(6,"YlOrRd"))
## gradient function ##
grad = function(f,x,h=0.01){
  n = length(x)
  delF = array(0,dim=c(n,1))
  for (i in 1:n){
    xhp = xhm = x
    
    xhp[i]=xhp[i]+h
    xhm[i]=xhm[i]-h
    delF[i] = (f(xhp)-f(xhm))/(2*h)
  }
  return (delF)
}

## Hessian Function ##
hessian = function(f,x,hs=0.01){
  n = length(x)
  H = array(0,dim=c(n,n))
  
  for (i in 1:n){
    h = array(0.0,dim=c(n,1))
    h[i] = hs
    H[i,]=t((grad(f,x+h)-grad(f,x-h))/(2*hs))
    #print(H[i,])
  }
  return (H)
}
## Step one of the algorithm ##
x0=c(500,500)
g=grad(P,x0)
H=hessian(P,x0)
print("Hessian")
print(H)
print("Negative Gradient")
print(-g)
print("Solve H*p=-g for p")
p = solve(H,-g)
print(p)
x1 = x0+p
print("Next x is")
print(x1)

## Step two of the algorithm ##
g=grad(P,x1)
H=hessian(P,x1)
print(H)
print(-g)
print("Solve H*p=-g for p")
p = solve(H,-g)
print(p)
x2 = x1+p
print("Next x is")
print(x2)

## Step 3 of the algorithm ##
g=grad(P,x2)
H=hessian(P,x2)
print(H)
print(-g)
print("Solve Hp=-g")
p = solve(H,-g)
print(p)
x3 = x2+p
print("Next x is")
print(x3)

p = function(x){((600-3*x[1]+x[2])+(800-2*x[2]+x[1]))-(200*x[1]+300*x[2])*-1}
x = c(100,200)
ans = optim(x,p,method = "L-BFGS_B")
print(ans$par) #values of x[1] and x[2]
print(-ans$val)

Units = function(x){c((600-3*x[1]+x[2])+(800-2*x[2]+x[1]))}
p = function(x){(x[1]*Units(x)[1]+x[2]*Units(x)[2]-(200*x[1]+300*x[2]))*(-1)}
x = c(100,100)
Units(c(100,100))
ans = optim(x,p)
Units(ans$par)

cost1 = seq(200,600,20)
Profit = function(c1){
  p = function(x){(x[1]*Units(x)[1]+x[2]*Units(x)[2]-(c1*Units(x)[1]+300*Units(x)[2]))*(-1)}
  x = c(100,100)
  ans = optim(x,p)
  return(ans)
}
Profit(250)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0
for (i in 1: length (cost1)){
  ans = Profit(cost1[i])
  ans.profit = -ans$value
  ans.x1 = Units(ans$par)[1]
  ans.x2 = Units(ans$par)[2]
  
}


