library(ggplot2)
library(gridExtra)
########################################################################
########################################################################
#### Chisquared Independence Test
########################################################################
########################################################################
#################################
# Load data
#################################
R1<-c(291,375,131,216,19)
R2<-c(111,152,58, 82, 14)
R3<-c(65, 86, 28, 51, 14)

Iran.tab<-matrix(data=c(O1,O2,O3),
                 nrow = 3,
                 ncol = 5,
                 byrow = TRUE)
colnames(Iran.tab)<-c("Strongly support","Somewhat support","Somewhat oppose","Strongly oppose","No opinion")
rownames(Iran.tab)<-c("Some college or less","Bachelor degree", "Post-graduate")
Iran.tab

(row.sums<-rowSums(Iran.tab))
(col.sums<-colSums(Iran.tab))
(obs<-sum(Iran.tab))


#################################
# Chi squared test `by hand`
#################################
## Expected counts under the null
(E.R1<-row.sums[1]*col.sums/obs)
(E.R2<-row.sums[2]*col.sums/obs)
(E.R3<-row.sums[3]*col.sums/obs)


(Iran.Etab<-matrix(data=c(E.R1,E.R2,E.R3),
                   nrow = 3,
                   ncol = 5,
                   byrow = TRUE))

## Test statistics
(test.stat<-sum((Iran.tab-Iran.Etab)^2/Iran.Etab))

## p-value
1-pchisq(q=test.stat,df=8)

#################################
# Chi squared test visualization
#################################
alpha<-0.05 #significance level
v<-(nrow(Iran.tab) - 1)*(ncol(Iran.tab) - 1)
ggdat<-data.frame(chisq=seq(from=0,to=30,by=0.01),
                  f=dchisq(seq(from=0,to=30,by=0.01),df=v))
#plot a point at the 0.95 quantile
ggdat.highlight<-data.frame(x=test.stat,
                            y=0)
ggplot(data=ggdat,aes(x=chisq,y=f))+
  geom_line()+
  geom_ribbon(data=subset(ggdat,chisq>=qchisq(1-alpha,df=v)),aes(ymax=f),ymin=0,
              fill="grey",color=NA,alpha=0.5)+
  geom_ribbon(data=subset(ggdat,chisq>=test.stat),aes(ymax=f),ymin=0,
              fill="red",color=NA,alpha=0.25)+
  geom_point(data=ggdat.highlight,aes(x=x,y=y),color="red")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  xlab(bquote(chi^2))+
  ylab("Density")+
  ggtitle("Chi-square Independence Test",
          subtitle=bquote(H[0]*": Independence versus "*H[a]*": Dependence"))+
  annotate("text",x=20,y=0.015,label= deparse(bquote(alpha==0.05)),parse=TRUE,size=3.5)+ 
  annotate("text",x=7,y=0.05,label= deparse(bquote(1-alpha==0.95)),parse=TRUE,size=3.5)+
  annotate("text",x=11,y=0.005,label= "Observation",size=3.5)+
  annotate("text",x=15.5,y=0.04,label= "p-value=0.1158",size=3.5)

qchisq(p=0.95,df=8)
#################################
# Chi squared test in R
#################################
chisq.test(Iran.tab)

fisher.test(Iran.tab)
fisher.test(Iran.tab,simulate.p.value=TRUE,B=1000)

chisq.test(tea.tab)

#################################
# Cramer's V
#################################
chisq.test(Iran.tab,correct=FALSE)
sqrt((chisq.test(Iran.tab,correct=FALSE)$statistic/sum(Iran.tab))/min(nrow(Iran.tab)-1,ncol(Iran.tab)-1))

cramerV(Iran.tab,bias.correct = TRUE)
cramerV(Iran.tab)
library(RVAideMemoire)
cramer.test(Iran.tab,conf.level = 0.95) ##bootstrapping inference for cramer's V
