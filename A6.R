
linear_models<-1-0.5*x1+x2-0.5*x3+x4
probit_model<-1-0.5*x1+x2-0.5*x3+x4
sigma<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0.5,0,0,0.5,1),4,4)
sigma
#sigma1<-matrix(c)
mvec<-c(0,0,0,0)
n<-100
library(MASS)
require(aod)
X<-mvrnorm(n,mvec,sigma)
X1<-matrix(c(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1)),100,5)
Y_l<-1-0.5*X[,1]+X[,2]-0.5*X[,3]+X[,4]
Y_p<-1-0.5*X[,1]+X[,2]-0.5*X[,3]
Y_p<-(Y_p>0)+0
Y_p

q1a<-lm(Y_l~X[,1]+X[,3]+X[,4])
q1b<-lm(Y_l~X[,1]+X[,2]+X[,3])
q1c<-lm(Y_l~X[,1]+X[,2]+X[,3]+X[,4]+X1[,1]+X1[,2]+X1[,3]+X1[,4]+X1[,5])

q1d<-glm(Y_p~X[,1]+X[,3]+X[,4],family = binomial(link="probit"))
q1e<-glm(Y_p~X[,1]+X[,2]+X[,3],family = binomial(link="probit"))
q1f<-glm(Y_p~X[,1]+X[,2]+X[,3]+X[,4]+X1[,1]+X1[,2]+X1[,3]+X1[,4]+X1[,5],family = binomial(link="probit"))
summary(q1d)
summary(q1e)
summary(q1f)

library(ezsim)
X1<-mvrnorm(n,mvec,sigma)
ezsim(500, dgp=X, estimator = function(x) )
