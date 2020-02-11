library(ggplot2)
library(gridExtra)
########################################################################
########################################################################
#### Fisher's Test
########################################################################
########################################################################
########################################################################
#### Tea Example
########################################################################
###################
# Load Data
###################
O11<-3
O12<-1
O21<-1
O22<-3

tea.tab<-matrix(data=c(O11,O12,O21,O22),
                nrow = 2,
                ncol = 2,
                byrow = TRUE)
colnames(tea.tab)<-c("Tea.First","Milk.First")
rownames(tea.tab)<-c("Guessed.Tea.First","Guessed.Milk.First")
tea.tab

###################
# correlation
###################
library(rcompanion)
phi(tea.tab)

###################
# Cramer's $V$ 
# which comes after
# the chi-squared test
###################
chisq.test(tea.tab,correct=FALSE)
sqrt((chisq.test(tea.tab,correct=FALSE)$statistic/sum(tea.tab))/min(nrow(tea.tab)-1,ncol(tea.tab)-1))

cramerV(tea.tab,bias.correct = TRUE)
cramerV(tea.tab)
library(RVAideMemoire)
cramer.test(tea.tab,conf.level = 0.95) ##bootstrapping inference for cramer's V

###################
# Mantel-Haenszel odds ratio
###################
(OR.mh<-(O11/O21)/(O12/O22))

###################
# CMLE odds ratio
###################
OR.lh<-function(OR,neg=FALSE){
  a=max(0,4-4)
  b=min(4,4)
  seq<-seq(a,b)
  denom=0
  for(u in seq){
    denom=denom+choose(4,u)*choose(4,4-u)*OR^u
  }
  lh=choose(4,3)*choose(4,1)*OR^3/denom
  ifelse(!neg,lh,-lh)
}

(OR.cmle<-optim(f=OR.lh,
                par=9,
                method = "Brent",
                lower=0,
                upper=100,
                neg=TRUE)$par)

###################
# Hypergeometric Distribution
###################
ggdat<-data.frame(x=(-1:5),
                  f1=dhyper(x=(-1:5),m=4,n=4,k=4),
                  F1=phyper(q=(-1:5),m=4,n=4,k=4))
#####1
g1<-ggplot(data=ggdat,aes(x=x))+
  geom_linerange(aes(ymax=f1), ymin=0)+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlim(0,5)+
  ylim(0,0.55)+
  xlab("X")+
  ylab(bquote(f[x](x)))+
  ggtitle("Hypergeometric PMF",subtitle="m=4, n=4, k=4")

ggdat.openpoints<-data.frame(x=ggdat$x,
                             y=phyper(ggdat$x-1,m=4,n=4,k=4))
ggdat.closedpoints<-data.frame(x=ggdat$x,
                               y=phyper(ggdat$x,m=4,n=4,k=4))
g1.CDF<-ggplot(data=ggdat, aes(x = x, y = F1)) +
  geom_step()+
  geom_point(data = ggdat.openpoints, aes(x = x, y = y),shape=1) +
  geom_point(data = ggdat.closedpoints, aes(x = x, y = y)) +
  geom_hline(yintercept=0)+
  theme_bw()+
  xlab("X")+
  ylab(bquote(F[x](x)))+
  ggtitle("Hypergeometric CDF",subtitle="m=4, n=4, k=4")
grid.arrange(g1,g1.CDF,ncol=2)

###################
# p-value
###################
dhyper(x=3,m=4,n=4,k=4)+phyper(q=3,m=4,n=4,k=4,lower.tail=FALSE)

alpha<-0.05
qhyper(p=1-alpha,m=4,n=4,k=4) 

dhyper(x=3,m=4,n=4,k=4)+phyper(q=3,m=4,n=4,k=4,lower.tail=FALSE)
phyper(q=3,m=4,n=4,k=4,lower.tail=FALSE)



1-phyper(q=3,m=4,n=4,k=4) #P(X>3) our rejection region is any observation
#       greater than 3 (not including 3)
#       so we reject for X>3 (X>=4)
#Note: alpha=0.05 is not possible due
#      to the discrete sampling distribution

ggdat.highlight<-data.frame(x=3:4,
                            y=dhyper(x=(3:4),m=4,n=4,k=4))
ggplot(data=ggdat,aes(x=x))+
  geom_linerange(aes(ymax=f1), ymin=0)+
  geom_ribbon(data=subset(ggdat,x>=4),aes(ymax=f1),ymin=0,
              fill="grey",color=NA,alpha=0.5)+
  geom_linerange(data=ggdat.highlight,aes(ymax=y), ymin=0,color="red")+
  geom_hline(yintercept=0)+
  theme_bw()+
  xlim(0,5)+
  ylim(0,0.55)+
  xlab("X")+
  ylab(bquote(f[x](x)))+
  ggtitle("Fisher's Test of Independence",
          subtitle=bquote({H[0]*":"~OR==1}~"versus"~H[a]*":"~OR>1))+
  annotate("text", x=4.5, y=0.05,
           label= deparse(bquote(alpha==0.0143)),parse=T,size=3.5)+ 
  annotate("text", x=3.5, y=0.20, label="P-value=0.2429",size=3.5)

###################
# In R
###################
fisher.test(x = tea.tab,alternative = "greater")
fisher.test(x = tea.tab)
