library("ggplot2")
library("gridExtra")
#################################################################################
#################################################################################
#### Kendall's Tau b
#################################################################################
#################################################################################
filt.rate<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
             159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
moisture<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
            79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6)
#################################################################################
##hypothesis test       #########################################################
#################################################################################
(r<-cor(filt.rate,moisture,method="kendall"))

#Kendall's by hand
x<-filt.rate
y<-moisture
rx<-rank(x=x)
ry<-rank(x=y)

kendall.tab<-cbind(x,y,rx,ry)
kendall.tab<-kendall.tab[order(x),]
n<-nrow(kendall.tab)

concordant<-c() #a place to save concordant pair counts
discordant<-c() #a place to save discordant pair counts
for(i in 1:n){ #for each row
  curr.x<-kendall.tab[i,3] #take the ith row's x rank
  curr.y<-kendall.tab[i,4] #take the ith row's y rank
  concordant_pairs<-c(which(kendall.tab[i:n,3]>curr.x&kendall.tab[i:n,4]>curr.y),
                      which(kendall.tab[i:n,3]<curr.x&kendall.tab[i:n,4]<curr.y))
  discordant_pairs<-c(which(kendall.tab[i:n,3]>curr.x&kendall.tab[i:n,4]<curr.y),
                      which(kendall.tab[i:n,3]<curr.x&kendall.tab[i:n,4]>curr.y))
  concordant<-c(concordant,length(concordant_pairs))
  discordant<-c(discordant,length(discordant_pairs))
}
kendall.tab<-cbind(kendall.tab,concordant,discordant)
sum(concordant) #total concordant
sum(discordant) #total discordant
tab<-table(x,y)
Tx<-rowSums(tab)
Ty<-colSums(tab)
(sum(concordant)-sum(discordant))/sqrt((((n*(n-1)/2)-sum(Tx*(Tx-1)/2))*((n*(n-1)/2)-sum(Ty*(Ty-1)/2))))

sig2<-(1/18)*((n^2-n)*(2*n+5)-sum(Tx*(Tx-1)*(2*Tx+5))-sum(Ty*(Ty-1)*(2*Ty+5)))+
  (sum(Tx*(Tx-1)*(Tx-2))*sum(Ty*(Ty-1)*(Ty-2)))/(9*(n^2-n)*(n-2))+
  (sum(Tx*(Tx-1))*sum(Ty*(Ty-1)))/(2*(n^2-n))

sqrt(sig2)

S<-sum(concordant)-sum(discordant)
(test.stat<- (S)/sqrt(sig2))
(test.stat.corrected<- (S+(-1*sign(S)))/sqrt(sig2))
pnorm(test.stat.corrected,lower.tail=FALSE)

#approximate unless for n<1290 (no ties)
cor.test(filt.rate,moisture,method="kendall",alternative="greater")
cor.test(filt.rate,moisture,method="kendall",alternative="greater",continuity=TRUE)

################################
## Visualization
################################
ggdat<-data.frame(z=seq(-4.5,4.5,0.01),
                  f=dnorm(x=seq(-4.5,4.5,0.01)))
ggdat.highlight<-data.frame(x=test.stat,y=0)

#Save the observations corresponding to z
#S=r*sqrt(((n^2-n)-sum(Tx*(Tx-1)))*((n^2-n)-sum(Ty*(Ty-1))))
solve_tau<-function(kendalls.tab,z,tau,correct=TRUE){
  kendalls.tab<-data.frame(kendalls.tab)
  tab<-table(kendalls.tab$x,kendalls.tab$y)
  n<-nrow(tab)
  Tx<-rowSums(tab)
  Ty<-colSums(tab)
  (sum(kendalls.tab$concordant)-sum(kendalls.tab$discordant))/sqrt((((n*(n-1)/2)-sum(Tx*(Tx-1)/2))*((n*(n-1)/2)-sum(Ty*(Ty-1)/2))))
  
  sig2<-(1/18)*((n^2-n)*(2*n+5)-sum(Tx*(Tx-1)*(2*Tx+5))-sum(Ty*(Ty-1)*(2*Ty+5)))+
    (sum(Tx*(Tx-1)*(Tx-2))*sum(Ty*(Ty-1)*(Ty-2)))/(9*(n^2-n)*(n-2))+
    (sum(Tx*(Tx-1))*sum(Ty*(Ty-1)))/(2*(n^2-n))
  
  S<-tau*sqrt(((n^2-n)-sum(Tx*(Tx-1)))*((n^2-n)-sum(Ty*(Ty-1))))
  if(correct==TRUE){
    (S+(-1*sign(S)))/sqrt(sig2) - z
  }else{
    (S)/sqrt(sig2) - z
  }
}
alpha<-0.05
critical<-uniroot(f=solve_tau,interval=c(0,1),
                  kendalls.tab=kendall.tab,z=qnorm(1-alpha),correct=TRUE)$root


axis.labels<-c(round(c(0,critical,r),4))

ggplot(data=ggdat,aes(x=z,y=f))+
  geom_line()+
  geom_ribbon(data=subset(ggdat,z>=qnorm(1-alpha)),aes(ymax=f),ymin=0,
              fill="grey",color=NA)+
  geom_ribbon(data=subset(ggdat,z>=test.stat),aes(ymax=f),ymin=0,
              fill="red",color=NA,alpha=0.25)+
  geom_point(data=ggdat.highlight,aes(x=x,y=y),color="red")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  xlab("z")+
  ylab("Density")+
  ggtitle("Z Test for the Populaiton Kendall's Tau-b",
          subtitle=bquote(H[0]*":"~rho[tau]==0*{", versus "*H[a]*":"~rho[tau]>0}))+
  annotate("text", x=2.5, y=0.07,
           label= deparse(bquote(alpha==0.05)),parse=T,size=3.5)+ 
  annotate("text", x=4.25, y=0.05, label="Observation\n p-value<0.0001",size=3.5)+
  scale_x_continuous(sec.axis = sec_axis(~., 
                                         breaks=c(0,qnorm(1-alpha),test.stat),
                                         labels = axis.labels,name="Kendall's Tau-b Correlation"))

#################################################################################
##Permutation Test   #########################################################
#################################################################################
kendall.perm.test <- function(x,y,R=1000,alternative="two.sided",supplyCorrs=FALSE){
  n = length(x) #length(x)=length(y)
  if(n!=length(y)){
    stop("\n `x' and `y' must have the same length")
  }
  r = cor(x,y,method="kendall") #sample correlation
  corrs<-rep(NA,R) #a place to store correlations on resampled
  for(i in 1:R){
    curr.samp<-sample(x=1:n,size=n,replace=FALSE) #randomly generate observations to sample
    corrs[i]<-cor(x,y[curr.samp],method="kendall") #save correlation for this sample
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
perm.test<-kendall.perm.test(filt.rate,moisture,supplyCorrs=TRUE)
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
  xlab("Kendall's Tau-b Correlations")+
  ylab("Density")+
  ggtitle("Permutation Test For Kendall's Tau-b Correlation",
          subtitle=bquote(H[0]*":"~rho[tau]==0*{", versus "*H[a]*":"~rho[tau]>0}))

#################################################################################
##confidence interval   #########################################################
#################################################################################
#Bonett and Wright (2000). 
zr<-(1/2)*log((1+r)/(1-r))
zl<-zr-qnorm(0.925)*sqrt(0.437/(20-4))
zu<-zr+qnorm(0.975)*sqrt(0.437/(20-4))

(rl<-(exp(2*zl)-1)/(exp(2*zl)+1))
(ru<-(exp(2*zu)-1)/(exp(2*zu)+1))

cor.test(filt.rate,moisture,method="kendall",continuity=TRUE)


#######################################################
###If data is not normal:
#######################################################
library("boot")
boot.kendall<-function(data,indices){
  d<-data[indices,]
  return(cor(d$filt.rate,d$moisture,method="kendall"))
}
boot.dat<-data.frame(filt.rate=filt.rate,moisture=moisture)
boot<-boot(boot.dat,R=1000,statistic=boot.kendall)
boot.ci(boot.out=boot,conf=0.95,type="perc")
