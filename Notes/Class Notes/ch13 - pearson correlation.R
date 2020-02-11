library("ggplot2")
library("gridExtra")
#################################################################################
#################################################################################
#### Pearson
#################################################################################
#################################################################################
filt.rate<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
             159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
moisture<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
            79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6)
#################################################################################
##hypothesis test       #########################################################
#################################################################################
library(energy)
mvnorm.etest(x=cbind(filt.rate,moisture),R=1000)

n<-length(filt.rate)
#Kendall, M. G., Stuart, A. (1973) 
#holds approximately for non-normal data
(r<-cor(filt.rate,moisture,method="pearson"))

(t.obs<-r*sqrt((n-2)/(1-r^2)))
pt(t.obs,df=n-2,lower.tail=FALSE)

cor.test(filt.rate,moisture,method="pearson",alternative="greater")

#################################
# Visualization
#################################
alpha<-0.05
ggdat<-data.frame(t=seq(-4.5,4.5,0.01),
                  f=dt(x=seq(-4.5,4.5,0.01),df=n-2))
ggdat.highlight<-data.frame(x=t.obs,y=0)

#Save the observations corresponding to t
#solving t=r*sqrt(18/(1-r^2)) for r yields: r = t/sqrt(t^2 + 18)
axis.labels<-round(c(-abs(r),qt(0.05,df=n-2)/sqrt(qt(0.05,df=n-2)^2 + (n-2)),0,
                     qt(0.95,df=n-2)/sqrt(qt(0.95,df=n-2)^2 + (n-2)),r),3)

ggplot(data=ggdat,aes(x=t,y=f))+
  geom_line()+
  geom_ribbon(data=subset(ggdat,t>=qt(1-alpha,df=n-2)),aes(ymax=f),ymin=0,
              fill="grey",color=NA)+
  geom_ribbon(data=subset(ggdat,t>=abs(t.obs)),aes(ymax=f),ymin=0,
              fill="red",color=NA,alpha=0.25)+
  geom_point(data=ggdat.highlight,aes(x=x,y=y),color="red")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  xlab("t")+
  ylab("Density")+
  ggtitle("T Test for the Population Pearson Correlation",
          subtitle=bquote(H[0]*":"~rho==0*{", versus "*H[a]*":"~rho>0}))+
  annotate("text", x=4, y=0.05,
           label= deparse(bquote(alpha==0.05)),parse=T,size=3.5)+ 
  annotate("text", x=8.2, y=0.05, label="Observation\nP-value<0.0001",size=3.5)+
  scale_x_continuous(sec.axis = sec_axis(~., 
                                         breaks=c(-abs(t.obs),qt(alpha,n-1),0,qt(1-alpha,n-1),abs(t.obs)),
                                         labels = axis.labels,name="Pearson Correlation"))

#################################
# Permutation test
#################################
pearson.perm.test <- function(x,y,R=1000,alternative="two.sided",supplyCorrs=FALSE){
  n = length(x) #length(x)=length(y)
  if(n!=length(y)){
    stop("\n `x' and `y' must have the same length")
  }
  r = cor(x,y,method="pearson") #sample correlation
  corrs<-rep(NA,R) #a place to store correlations on resampled
  for(i in 1:R){
    curr.samp<-sample(x=1:n,size=n,replace=FALSE) #randomly generate observations to sample
    corrs[i]<-cor(x,y[curr.samp],method="pearson") #save correlation for this sample
  }
  #calculate p-values as the proportion of correlations of resampled `more extreme` than observed
  if(alternative=="less"){
    p.value = sum(corrs <= r) / R 
  } else if(alternative=="greater"){
    p.value = sum(corrs >= r) / R
  } else{
    p.value = sum(abs(corrs) >= abs(r)) / R
  }
  if(supplyCorrs==TRUE){
    list(r=r,p.value=p.value,corrs=corrs)
  }else{
    list(r=r,p.value=p.value)
  }
}
perm.test<-pearson.perm.test(filt.rate,moisture,supplyCorrs=TRUE)
perm.test[1:2]

ggdat<-data.frame(corrs=perm.test$corrs)

ggplot(data=ggdat,aes(x=corrs))+
  geom_histogram(aes(y=..density..),
                 fill="lightblue",
                 color="black",
                 bins=20)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept=perm.test$r,color="red")+
  theme_bw()+
  xlab("Pearson Correlations")+
  ylab("Density")+
  ggtitle("Permutation Test For Pearson's Correlation",
          subtitle=bquote(H[0]*":"~rho==0*{", versus "*H[a]*":"~rho>0}))

#################################################################################
##confidence interval   #########################################################
#################################################################################
#Bonett and Wright (2000). 
cor.test(filt.rate,moisture,method="pearson")

###If data is not normal:
library("boot")
boot.pearson<-function(data,indices){
  d<-data[indices,]
  return(cor(d$filt.rate,d$moisture,method="pearson"))
}
boot.dat<-data.frame(filt.rate=filt.rate,moisture=moisture)
boot<-boot(boot.dat,R=1000,statistic=boot.pearson)
boot.ci(boot.out=boot,conf=0.95,type="perc")