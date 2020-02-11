mom<-rep(NA,1000)
mle<-rep(NA,1000)

for(i in 1:1000){
  x.unif<-runif(n=1000, min=0, max=3)

#MOM
g<-function(theta,x.data){
  b<-theta
  EX<- (0+b)/2 ##UNIFORM: E(X)=(a+b)/2
  EX-mean(x.data)
}
library("nleqslv")
mom[i]<-nleqslv(x= max(x.unif), #guess
        fn = g, #function solve =0
        x.data = x.unif)$x #data to use

#MLE
unif.ll<-function(x.data,theta,neg=FALSE){
  b<-theta
  ll<-sum(dunif(x=x.data,min=0,max = b,log=TRUE))
  ifelse(!neg,ll,-ll)
}
mle[i]<-optim(par = max(x.unif), #guess
      fn = unif.ll, #function to minimize
      method = "Brent", #univariate
      lower = 0,
      upper = 10,
      x.data=x.unif,
      neg=TRUE)$par

#mle_fixed[i]<-function{(1000+1)/1000 * max(x.unif)}
}
mean(mom)
mean(mle)

var(mom)
1/mean(mom)

var(mle)
1/mean(mle)

(mean(mom)-3)^2+var(mom)
(mean(mle)-3)^2+var(mle)

###CH 8
xbars<-rep(NA, 1000)
for (i in 1:1000){
  x<-rnorm(100,12,3)
  xbars[i]<-mean(x)
}

hist(xbars)
hist(xbars,probability=TRUE)
x<-seq(10,14,.001)
f<-dnorm(x,12,sd=sqrt(3^2/100))
lines(x,f,col="red")

for(i in 1:1000){
  x<-rpois(100,lambda=4)
  xbars[i]<-mean(x)
}
hist(xbars)
EX<-4
VARX<-var(4)/100
hist(xbars,probability=TRUE)
x<-seq(3,5,0.001)
f<-dnorm(x,mean=EX,sd=sqrt(VARX)/100)
lines(x,f,col='red')

############
inner_join(S,R,key) #key is the similar column in common, S, R, are the dataframes

############
