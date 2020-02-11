# Number 4
# Alexa's Attempt
clear
numofsatellites <- c(0:15)
numofobs <- c(62, 16, 9, 19, 19, 15, 13, 4, 6, 3, 3, 1, 1, 1, 1, 1)
crabby_data1 <- data.frame(numofobs,numofsatellites)
#sumofobs <- sum(numofobs)
#sample_mean=sum(numofobs * numofsatellites)/sumofobs

#create a vector of crab data
crabby_data <- c(rep(0, 62),
                 rep(1,16),
                 rep(2, 9),
                 rep(3, 19),
                 rep(4, 19),
                 rep(5,15),
                 rep(6, 13),
                 rep(7, 4),
                 rep(8, 6),
                 rep(9, 3),
                 rep(10,3),
                 rep(11, 1),
                 rep(12, 1),
                 rep(13, 1),
                 rep(14, 1),
                 rep(15, 1))


#install.packages("gmm") #install packages
library(gmm) #load packages
set.seed(69) #makes data comparable to others

#calculate the method of moments estimator for lambda

#2 moments = 2 parameters
#lambda = events/time * time period

# g<- function(theta,x) {
#   p = theta
#   m1 <- p - mean(x)
#   return(m1)
# }

g<- function(x,theta) { #(data, theta)
  #set the sample and population moments to be equal
  b <- theta
  m1 <- b - mean(x) #moment
  return(m1)
}

gmm(g=g,#function
    x=crabby_data,#data
    t0=mean(crabby_data), #inital guess
    method="Brent",#univariate
    lower=0, #reasonable lower
    upper=1.5*max(crabby_data)) #reasonable upper

#Part B - find maximum likelihood estimator for lambda
LL <- function(lambda) { #function inputting lambda
  return(-1*sum(dpois(crabby_data, lambda = lambda, log = TRUE))) #likelihood of distribution
}

a <- optim(fn = LL, #function
      par=mean(crabby_data), #best guess
      method="Brent", #
      lower=0, 
      upper=1.5*max(crabby_data))
a
#Part C - plot data of poisson distribution fit with MLE estimates
#library(ggplot2) #load library

ggdat<- data.frame(x=a,#MLE estimates
                   f1=dpois(x=a,lambda=2.977011)) #poisson distribution
ggplot() +
  geom_histogram(data = crabby_data1, aes(x=crabby_data, y=..density..),binwidth = 1,alpha=0.2,fill="purple") + #lay histogram
  #geom_linerange(data = test, (aes(x=x,ymax=f1)), ymin=0) +
  #stat_function(geom="line", fun = dpois) +
  stat_function(geom = "line", n = 15, fun = dpois, args = test)+
  geom_hline(yintercept=0)+ #horizontal line
  theme_bw()+ #removes grey background
  xlim(-1,15)+ #set x
  xlab("Number of Satellites")+
  ylab("Frequency")+
  ggtitle("Poisson Distribution",subtitle = "PMF Distribution for"~lambda==2.977011)


#Part D - zero-inflated poisson distribution
set.seed(69)

g_1<- function(x,theta) { #(data, theta)
  #set the sample and population moments equal
  lambda = theta[1] 
  sigma = theta[2]
  EX = (1-sigma)*lambda
  varX = (1-sigma) * (lambda + lambda^2) - ((1-sigma)*lambda)^2
  m1 <- EX - x
  m2 <- varX - (x-EX)^2
  return(c(m1, m2)) #return moments
}

gmm(t0= c(0,60), #best guess
    g=g_1,#function
    x=crabby_data) #data

#Part E - find the maximum likelihood estimator for lambda and sigma

#Steps
#turning the equation into a function that takes x, lambda, and sigma
#sum of the logs
#create another function that takes the data and theta, neg=FALSE
#sigma theta[1]
#lambda theta[2]
#ll <- sum,sapply (apply to data this function and the args sigma and lambda)

#optim guessing sigma is ,5 and 5
#fn is zero poisson
#data is crabby data
#neg=TRUE

alexa <-function(x, lambda, sigma) { #function given, probability of x given lambda and sigma
  log(((1-sigma)*(((lambda^x)*(exp(-lambda))/(factorial(x))))*(x>=1)) + ((sigma + (1 - sigma)*(exp(-lambda)))*(x==0)))
}

LL1 <- function(x, theta, neg=FALSE) {#takes the data and theta
  sigma<-theta[1]
  lambda<-theta[2]
  #applies a function to a vector and sums the values
  ll <-sum(sapply(X=crabby_data, FUN = alexa, sigma = sigma, lambda = lambda))
  ifelse(!neg,ll,-ll)
}

c <- optim(fn = LL1, #function
           x = crabby_data, #data
           par=c(min(crabby_data),max(crabby_data)), #best guess
           neg=TRUE)
c

#Part F - plot a histogram of zero-inflated poisson distribution fit

library(ggplot2)

alexa_non_log <-function(x, lambda, sigma) { #nonlog function to use for plotting
  (((1-sigma)*(((lambda^x)*(exp(-lambda))/(factorial(x))))*(x>=1)) + ((sigma + (1 - sigma)*(exp(-lambda)))*(x==0)))
}
#creating a distribution using sigma and lambda
zero_pois_dis <- sapply(X=0:15, FUN = alexa_non_log, sigma=0.3494643, lambda=4.5775109)
test1 <- data.frame(x=c(0:15), dist = zero_pois_dis)

install.packages("tidyverse") #install tidyvevrse
library(tidyverse) #load tidyverse
crabby_data1 <- tibble(crabby_data) #reformatting for histogram
#ggdat <- data.frame(x=c, f1=zero_pois_dis)
ggplot() +
  geom_histogram(data = crabby_data1, aes(x=crabby_data, y=..density..),binwidth = 1,alpha=0.2,fill="magenta")+ #histofobs
  geom_linerange(data = test1, aes(x=x, ymax=dist), ymin=0)+ #lines for distribution
  geom_hline(yintercept = 0)+ 
  theme_minimal()+
  xlim(-1,15)+
  xlab("Number of Satellites")+
  ylab("Number of Observations")+
  ggtitle("Zero-Inflated Poisson Distribution Fit", subtitle = "with MLE Estimates")
