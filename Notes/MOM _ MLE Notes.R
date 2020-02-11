clear

x.unif<-runif(1000,0,3)
#Method of moments
g <- function(x.data,theta) { #(data, theta)
  ###Set the sample and population
  ###moments to be equal
  b<-theta
  EX<-b/2
  EX-mean(x.data) #solve this =0 for b
}

library(nleqslv)
nleqslv(x=3, #initial guess
        fn=g,#function we want to solve
        x.data=x.unif) #our data
        # method="Brent", #univariate
        # lower=0, #reasonable lower
        # upper=10) #reasonable upper

#MLE
unif.ll<-function(x.data,theta,neg=FALSE){ #neg=FALSE assumes it's false unless it's true
  b<-theta #unknown parameter b
  ll<-sum(dunif(x=x.data,min=0,max=b,log=TRUE)) #likelihood of uniform distribution of our data w/ min=0 and max is our unknown parameter
  ifelse(!neg,ll,-ll) #return positive it FALSE, return negative if TRUE
}
optim(par=3, #best guess
      fn = unif.ll, #function to minimize
      x.data= x.unif, #data
      neg=TRUE, #when evaluating the fn, I'm using the negative log likelihood (maximizing likelihood = minimizing loglikelihood)
      method = "Brent",
      lower = 0,
      upper = 10)

y.unif<-runif(1000,14,634)
#Method of Moments
g<-function(x.data,theta){
  ###set the sample and population
  ###moments to be equal
  a<-theta[1]
  b<-theta[2]
  EX<-(a+b)/2
  VARX<-(b-a)^2/12
  eq1<-EX-mean(x.data)
  eq2<-VARX-var(x.data)
  return(c(eq1,eq2))
}

nleqslv(x=c(0,650),
        fn=g,
        x.data=y.unif)

#MLE
unif.ll<-function(x.data,theta,neg=FALSE){ #neg=FALSE assumes it's false unless it's true
  a<-theta[1]
  b<-theta[2] #unknown parameter b
  ll<-sum(dunif(x=x.data,min=a,max=b,log=TRUE)) #likelihood of uniform distribution of our data w/ min=0 and max is our unknown parameter
  ifelse(!neg,ll,-ll) #return positive it FALSE, return negative if TRUE
}
optim(par= c(0,650), #best guess
      fn = unif.ll, #function to minimize
      x.data= y.unif, #data
      neg=TRUE) #when evaluating the fn, I'm using the negative log likelihood (maximizing likelihood = minimizing loglikelihood)