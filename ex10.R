#mac sievert
#exercise 10
rm(list=ls())
setwd('/Users/Owner/Documents/1.0Notre Dame/UNDERC')
data=read.table('data2.txt',header=TRUE,sep=',')
#1.
#define linear function
linear=function(p,x,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  pred=B0+B1*x
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  return(nll)
}
#define quadradic function
quad=function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  sigma=exp(p[4])
  pred=B0+B1*x+B2*x^2
  nll=-sum(dnorm(x=y,mean=pred,sd=sigma,log=TRUE))
  return(nll)
}
#estimate parameters and optim
linearGuess=c(12,12,1)
quadGuess=c(12,12,12,1)

fitLinear=optim(par=linearGuess,fn=linear,x=data$x,y=data$y)
fitquad=optim(par=quadGuess,fn=linear,x=data$x,y=data$y)
#likelihod ratio test
teststat=2*(fitquad$value-fitLinear$value)

df=length(fitLinear$par)-length(fitquad$par)

1-pchisq(teststat,df=1)
#chisquared value is 1. that means the linear model is better for data.txt

#2.
library(deSolve)
library(ggplot2)
#define lotka-volterra competition model simulation
LV=function(t,y,p){
  n1=y[1]
  n2=y[2]
  r1=p[1]
  r2=p[2]
  a12=p[3]
  a11=p[4]
  a21=p[5]
  a22=p[6]
  
  dN1dt=r1*(1-n1*a11-n2*a12)*n1
  dN2dt=r2*(1-n2*a22-n1*a21)*n2
  
  return(list(c(dN1dt,dN2dt)))
}

#case1 a12>a11 and a21<a22, species 1 has a greater effect on species 2, species 1 outcompetes species 2
times=1:100
y0=c(0.5,0.5)
params=c(0.7,0.5,0.07,0.02,0.02,0.07)
sim=ode(y=y0,times=times,func=LV,parms=params)
out=data.frame(time=sim[,1],spec1=sim[,2],spec2=sim[,3])
ggplot(out,aes(x=time,y=spec1))+
  geom_line()+theme_classic()+
  geom_line(data=out,mapping=aes(x=time,y=spec2),color='green')+
  theme_classic()+ylab('species')

#case2 a12<a11 and a21>a22, species 2 has a greater effect on species 1 species 2 outcompetes species 1
times=1:100
y0=c(0.5,0.5)
params=c(0.5,0.7,0.02,0.07,0.07,0.02)
sim=ode(y=y0,times=times,func=LV,parms=params)
out=data.frame(time=sim[,1],spec1=sim[,2],spec2=sim[,3])
ggplot(out,aes(x=time,y=spec1))+
  geom_line()+theme_classic()+
  geom_line(data=out,mapping=aes(x=time,y=spec2),color='green')+
  theme_classic()+ylab('species')

#case2 a12<a11 and a21<a22, both species effect eachother, species 1 and 2 reach equilibrium and co-exist
times=1:100
y0=c(0.5,0.5)
params=c(0.5,0.7,0.02,0.07,0.02,0.07)
sim=ode(y=y0,times=times,func=LV,parms=params)
out=data.frame(time=sim[,1],spec1=sim[,2],spec2=sim[,3])
ggplot(out,aes(x=time,y=spec1))+
  geom_line()+theme_classic()+
  geom_line(data=out,mapping=aes(x=time,y=spec2),color='green')+
  theme_classic()+ylab('species')



