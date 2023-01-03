#Q1
#a
library(ggplot2)
train_data = read.table("hw4-nn-train-100.dat",header = F)
train_data = as.matrix(train_data)
X = train_data[,1:2]
Y = factor(train_data[,3],levels=c(-1,1),labels=c("-1","1"))
ggplot(dat, aes(x = X[,1], y = X[,2], colour = Y)) +  geom_point()   

#c
train_X = cbind(1,train_data[,1:2])
train_Y = train_data[,3]
train_Y = (1+train_Y)/2
n = length(train_Y)

Z_fun = function(x,W){
  Z = x %*% W
  Z = 1/(1+exp(-Z))
  Z = cbind(1,Z)
  return(t(Z))
}

F_fun = function(x,B,W){
  Z = Z_fun(x,W)
  U = t(Z) %*% B
  F = 1/(1+exp(-U))
  return(F)
}

L_fun = function(X,Y,B,W,n){
  C = 0
  for(i in 1:n){
    f = F_fun(X[i,],B,W)
    C = C - Y[i]*log(f) - (1-Y[i])*log(1-f)
  }
  L = C/n
  return(L)
}

L_fun_01 = function(X,Y,B,W,n){
  L = 0
  for(i in 1:n){
    f = F_fun(X[i,],B,W)
    if(abs(f-Y[i])>0.5){
      L = L+1
    }
  }
  L = L/n
  return(L)
}

m = 8
tol = 10^(-4)
set.seed(666)
W = matrix(rnorm(3*m,0,8),3,m)
B = matrix(rnorm(m+1,-1,5),m+1,1)
L_old = L_fun(train_X,train_Y,B,W,n) + 1
L_new = L_fun(train_X,train_Y,B,W,n)
a = 1
n_iter = 0
L_logit_train = c(L_new)
L_01_train = c(L_fun_01(train_X,train_Y,B,W,n))
L_logit_test = c(L_fun(test_X,test_Y,B,W,N))
L_01_test = c(L_fun_01(test_X,test_Y,B,W,N))
while((1-L_new/L_old)>tol){
  n_iter = n_iter + 1
  L_old = L_new
  dC_dB = matrix(0,m+1,1)
  dC_dW = matrix(0,3,m)
  for(i in 1:n){
    Z = Z_fun(train_X[i,],W)
    f = F_fun(train_X[i,],B,W)
    dC_dB = dC_dB + c(((f-train_Y[i])/(f*(1-f)))*((exp(-t(B)%*%Z)/(1+exp(-t(B)%*%Z))^2)))*Z
    for(j in 1:m){
      dC_dW[,j] = dC_dW[,j] + c(((f-train_Y[i])/(f*(1-f)))*(exp(-t(B)%*%Z)/(1+exp(-t(B)%*%Z))^2)*
                                  B[j]*(exp(-W[,j]%*%train_X[i,])/(1+exp(-W[,j]%*%train_X[i,]))^2))*train_X[i,]
    }
  }
  dL_dB = dC_dB/n
  dL_dW = dC_dW/n
  B = B - a*dL_dB
  W = W - a*dL_dW
  L_new = L_fun(train_X,train_Y,B,W,n)
  
  L_logit_train = append(L_logit_train,L_new)
  L_01_train = append(L_01_train,L_fun_01(train_X,train_Y,B,W,n))
  L_logit_test = append(L_logit_test,L_fun(test_X,test_Y,B,W,N))
  L_01_test = append(L_01_test,L_fun_01(test_X,test_Y,B,W,N))
}
L_01_train_final = L_fun_01(train_X,train_Y,B,W,n)
L_logit_train_final = L_new


#e
test_data = read.table("hw4-nn-test.dat")
test_data = as.matrix(test_data)
test_X = cbind(1,test_data[,1:2])
test_Y = (1+test_data[,3])/2
N = length(test_Y)

L_logit_test_final = L_fun(test_X,test_Y,B,W,N)
L_01_test_final = L_fun_01(test_X,test_Y,B,W,N)


#f
plot(L_logit_train, type = "l",col="red",xlab = "k", ylab="loss/cost",lty=1,lwd=2)
lines(L_logit_test, type = "l",col="green",lty=1,lwd=2)
legend("topright",legend=c("L_logit_train","C_logit_test"),col=c("red","green"),lty=1,lwd=2)

plot(L_01_train, type = "l",col="red",xlab = "k", ylab="loss/cost",lty=1,lwd=2)
lines(L_01_test, type = "l",col="green",lty=1,lwd=2)
legend("topright",legend=c("L_01_train","C_01_test"),col=c("red","green"),lty=1,lwd=2)


#g
x1 = seq(0,1,0.01)
x2 = seq(0,1,0.01)
fx = matrix(0,length(x1),length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    fx[i,j] = F_fun(c(1,x1[i],x2[j]),B,W)-1/2
  }
}
plot(X,col=(train_Y+2),xlab="x1",ylab="x2")
contour(x1,x2,fx,levels = 0,add=T)
