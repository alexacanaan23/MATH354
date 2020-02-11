library("ggplot2")
library("gridExtra")
#################################################################################
#################################################################################
#### Spearman's Rank
#################################################################################
#################################################################################
filt.rate<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
             159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
moisture<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
            79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6)
#################################################################################
##hypothesis test       #########################################################
#################################################################################
#Press; Vettering; Teukolsky; Flannery (1992)
n<-length(filt.rate)
(r<-cor(filt.rate,moisture,method="spearman"))
(t.obs<-r*sqrt((n-2)/(1-r^2)))
pt(t.obs,df=n-2,lower.tail=FALSE)

#corrected
(r<-cor(filt.rate,moisture,method="spearman"))
r.adjusted<-1-((n^3-n)*(1-r)/6)/((n*(n^2-1))/6+1)
t.obs<-(r.adjusted / sqrt((1 - r.adjusted^2)/(n-2)))
pt(t.obs,df=n-2,lower.tail=FALSE)

#approximate unless for n<1290 (no ties)
cor.test(filt.rate,moisture,method="spearman",alternative="greater")
cor.test(filt.rate,moisture,method="spearman",alternative="greater",continuity = TRUE)

#################################
# Visualization
#################################
alpha<-0.05
ggdat<-data.frame(t=seq(-4.5,4.5,0.01),
                  f=dt(x=seq(-4.5,4.5,0.01),df=n-2))
ggdat.highlight<-data.frame(x=t.obs,y=0)

#Save the observations corresponding to t
#solving t=r*sqrt(18/(1-r^2)) for r yields: r = t/sqrt(t^2 + 18)
axis.labels<-round(c(0,qt(0.95,df=n-2)/sqrt(qt(0.95,df=n-2)^2 + (n-2)),r),3)

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
  ggtitle("T Test for the Population Spearman Correlation",
          subtitle=bquote(H[0]*":"~rho[s]==0*{", versus "*H[a]*":"~rho[s]>0}))+
  annotate("text", x=4, y=0.05,
           label= deparse(bquote(alpha==0.05)),parse=T,size=3.5)+ 
  annotate("text", x=6.5, y=0.05, label="Observation\nP-value<0.0001",size=3.5)+
  scale_x_continuous(sec.axis = sec_axis(~., 
                                         breaks=c(0,qt(1-alpha,n-1),t.obs),
                                         labels = axis.labels,name="Spearman Correlation"))

#################################
# Permutation test
#################################
spearman.perm.test <- function(x,y,R=1000,alternative="two.sided",supplyCorrs=FALSE){
  n = length(x) #length(x)=length(y)
  if(n!=length(y)){
    stop("\n `x' and `y' must have the same length")
  }
  r = cor(x,y,method="spearman") #sample correlation
  corrs<-rep(NA,R) #a place to store correlations on resampled
  for(i in 1:R){
    curr.samp<-sample(x=1:n,size=n,replace=FALSE) #randomly generate observations to sample
    corrs[i]<-cor(x,y[curr.samp],method="spearman") #save correlation for this sample
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
perm.test<-spearman.perm.test(filt.rate,moisture,supplyCorrs=TRUE)
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
  xlab("Spearman Correlations")+
  ylab("Density")+
  ggtitle("Permutation Test For Spearmans's Correlation",
          subtitle=bquote(H[0]*":"~rho[s]==0*{", versus "*H[a]*":"~rho[s]>0}))

#################################################################################
##confidence interval   #########################################################
#################################################################################
#Bonett and Wright (2000). 
zr<-(1/2)*log((1+r)/(1-r))
zl<-zr-qnorm(0.925)*sqrt((1+r^2/2)/(20-3))
zu<-zr+qnorm(0.975)*sqrt((1+r^2/2)/(20-3))

(rl<-(exp(2*zl)-1)/(exp(2*zl)+1))
(ru<-(exp(2*zu)-1)/(exp(2*zu)+1))

cor.test(filt.rate,moisture,method="spearman")

###If data is not normal:
library("boot")
boot.spearman<-function(data,indices){
  d<-data[indices,]
  return(cor(d$filt.rate,d$moisture,method="spearman"))
}
boot.dat<-data.frame(filt.rate=filt.rate,moisture=moisture)
boot<-boot(boot.dat,R=1000,statistic=boot.spearman)
boot.ci(boot.out=boot,conf=0.95,type="perc")
