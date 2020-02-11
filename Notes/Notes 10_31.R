library("MASS")
data("Animals")
head(Animals)
hist(Animals$brain)
t.test(x=Animals$brain, conf.level = 0.95)
#for u is (56.9,1092.2)
#what about M (population Median)?
#CI: We'll use resampling

R<-1000
medians<-rep(NA,1000)
length(Animals$brain) #28
for (i in 1:R){
  x.samp<-sample(x=Animals$brain, size= 28, replace=TRUE)
  medians[i]<-median(x.samp)
}
hist(medians)

quantile(medians, probs=c(0.025,0.975))

library(boot)
boot.median<-function(data,indices){
  d<-data[indices]  # allows boot to select sample
  return(median(d))
}

boot.IQR<-function(data,indices){
  d<-data[indices]  # allows boot to select sample
  return(IQR(d))
}
animalboot<-boot(Animals$brain,R=1000,statistic=boot.median) #1:1000
boot.ci(boot.out=animalboot,conf=0.95,type="perc")#calculates CI


animalboot<-boot(Animals$brain,R=1000,statistic=boot.IQR) #1:1000
boot.ci(boot.out=animalboot,conf=0.95,type="perc")

##################################################
#copper shows up in wholemeal flour
#obs that ield a igher amount of copper than a typical sample
data(chem)
chem
hist(chem)

# u=4, u >4
t.test(chem, mu=4, conf.level = .95,alternative = "greater")
#t=0.259
#p=0.3988>0.05
#fail to reject Ho, not sufficient evidence for u > 4

#M=4, M>4
#B=2
(B=length(which(chem>4))) #2
#pvalue = P(B>2) = 1- P(B<=2) = 1-pbinom(q=2,size=n,prob=.5)
1-pbinom(q=B,size=length(chem),prob=.5)
install.packages("BSDA")
library("BSDA")
SIGN.test(x=chem,
          md = 4,
          alternative = "greater",
          conf.level = 0.95)
library(ggplot2)
library(gridExtra)
