#########################################################################
### Ovarian Cancer Example
#########################################################################
library(survival)
data(ovarian)
ovarian$rx<-ifelse(ovarian$rx==1,"Cyclophosphamide Alone",
                   "Cyclophosphamide Plus Adriamycin")
surv_obj<-Surv(time=ovarian$futime,event=ovarian$fustat)
fit<-survfit(surv_obj~rx,data=ovarian)
library(survminer)
ggsurvplot(fit,data=ovarian,xlab="Survival Time (days)")

ovarian.nodropouts<-ovarian[which(ovarian$fustat==0),]
ggdat<-data.frame(rx=ovarian.nodropouts$rx,age=ovarian.nodropouts$age)
ggplot(data=ggdat,aes(x=rx, y=age))+
  geom_violin(fill="lightblue")+
  geom_boxplot(width=0.25)+
  theme_bw()+
  xlab("Treatment")+
  ylab("Age (years)")+
  ggtitle("Age of Participants by Treatment Group",
          subtitle="For non-dropouts")

ggdat<-data.frame(age=ovarian.nodropouts$age,futime=ovarian.nodropouts$futime)
ggplot(data=ggdat,aes(x=age, y=futime))+
  geom_point(shape=1)+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  xlab("Age (years)")+
  ylab("Survival Time (days)")

#Spearman by hand
n<-nrow(ovarian.nodropouts)
x<-ovarian.nodropouts$age
y<-ovarian.nodropouts$futime
rx<-rank(x=x)
ry<-rank(x=y)
di<-ry-rx
di_squared<-di^2
(rs_table<-cbind(x,y,rx,ry,di,di_squared))
sum(di_squared)
1- 6*sum(di_squared)/(n*(n^2-1))

#Asking R
cor(x,y,method="spearman")
cor(x,y,method="spearman")

#Kendall's by hand
x<-ovarian.nodropouts$age
y<-ovarian.nodropouts$futime
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
(kendall.tab<-cbind(kendall.tab,concordant,discordant)) #creates table
sum(concordant) #total concordant
sum(discordant) #total discordant
tab<-table(x,y)
Tx<-rowSums(tab)
Ty<-colSums(tab)
(sum(concordant)-sum(discordant))/sqrt((((n*(n-1)/2)-sum(Tx*(Tx-1)/2))*((n*(n-1)/2)-sum(Ty*(Ty-1)/2))))


#Asking R
cor(x,y,method="kendall")
StuartTauC(x,y)

#################################################################################
#################################################################################
# Doctor Example
#################################################################################
#################################################################################
library("AER")
data("DoctorVisits")
n<-nrow(DoctorVisits)
x<-DoctorVisits$illness
y<-DoctorVisits$visits
(tab<-table(x,y))
cor(x,y,method="spearman")
cor(x,y,method="kendall")
StuartTauC(x,y)
#Thinking Discrete?: cramerV(x,y)

##Interesting Unbiased Versions:
##https://www.eee.hku.hk/~yhhou/pdf/1-s2.0-S0165168412002721-main.pdf
# rs<-cor(x,y,method="spearman")
# rs.unbiased<-2*sin(pi/6 * rs)
# rk<-cor(x,y,method="kendall")
# rk.unbiased<-sin(pi/2 * rk)
# #weighted average
# rm<-2*sin((pi/6)*rs-(pi/2)*((rk-rs)/(n-2)))

#################################################################################
####Fact1: Scatterplots##########################################################
#################################################################################
generateCorrelatedData2<-function(sign,sd){
  x<-runif(n,0,20)
  y<- log(x^sign)+ rnorm(n=n,mean=0,sd=sd*sd(y))
  corr_rho<-round(cor(x,y,method="spearman"),2)
  corr_taub<-round(cor(x,y,method="kendall"),2)
  corr_tauc<-round(StuartTauC(x,y),2)
  data.frame(x=x,y=y,
             rho=rep(corr_rho,length(x)),
             taub=rep(corr_taub,length(x)),
             tauc=rep(corr_tauc,length(x)))
}


corr.plots<-list()
sds<-c(0.01,0.50,1,2.5,0.01,0.50,1,2.5)
for(i in 1:length(ms)){
  dir<-ifelse(i<5,1,-1)
  ggdat<-generateCorrelatedData2(dir,sds[i])
  corr.plots[[i]]<-ggplot(data=ggdat,aes(x=x,y=y))+
    geom_point(shape=1)+
    geom_smooth(alpha=0.25,color="black",method="loess")+
    theme_bw()+
    ggtitle(bquote({{r[s]==.(ggdat$rho[1])}*", "*tau["b"]==.(ggdat$taub[1])}*", "*tau["c"]==.(ggdat$tauc[1])))+
    theme(plot.title = element_text(size=7.5)) #changes title font size
}
grid.arrange(corr.plots[[1]],corr.plots[[2]],corr.plots[[3]],corr.plots[[4]],
             corr.plots[[5]],corr.plots[[6]],corr.plots[[7]],corr.plots[[8]],ncol=4)

#################################
# Fact 3
#################################
survival_in_weeks<-ovarian.nodropouts$futime/7
cor(ovarian.nodropouts$age,ovarian.nodropouts$futime,method="spearman")
cor(ovarian.nodropouts$age,survival_in_weeks,method="spearman")
cor(ovarian.nodropouts$age,ovarian.nodropouts$futime,method="kendall")
cor(ovarian.nodropouts$age,survival_in_weeks,method="kendall")
StuartTauC(ovarian.nodropouts$age,ovarian.nodropouts$futime)
StuartTauC(ovarian.nodropouts$age,survival_in_weeks)

#################################
# Fact 4
#################################
cor(ovarian.nodropouts$age,ovarian.nodropouts$futime,method="spearman")
cor(ovarian.nodropouts$futime,ovarian.nodropouts$age,method="spearman")
cor(ovarian.nodropouts$age,ovarian.nodropouts$futime,method="kendall")
cor(ovarian.nodropouts$futime,ovarian.nodropouts$age,method="kendall")
StuartTauC(ovarian.nodropouts$age,ovarian.nodropouts$futime)
StuartTauC(ovarian.nodropouts$futime,ovarian.nodropouts$age)


n<-nrow(DoctorVisits)
x<-DoctorVisits$illness
y<-DoctorVisits$visits
cor(x,y,method="spearman")
cor(y,x,method="spearman")
cor(x,y,method="kendall")
cor(y,x,method="kendall")
StuartTauC(x,y)
StuartTauC(y,x)

#################################
# Fact 5
#################################
library("Ecdat")
data(Caschool)
cor(Caschool$mealpct,Caschool$mathscr,method = "spearman")
cor(Caschool$mealpct,Caschool$mathscr,method = "kendall")
#StuartTauC(Caschool$mealpct,Caschool$mathscr) #this takes very long

#################################
# Fact 6
#################################
filt.rate<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
             159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
moisture<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
            79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6)
filt.rate2<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
              159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7,
              80)#outlier added
moisture2<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
             79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6,
             81)#outlier added

ggdat<-data.frame(filt.rate=filt.rate2,moisture=moisture2)
ggdat.highlight<-data.frame(x=80,y=81)
ggplot(ggdat, aes(x=filt.rate, y=moisture)) +
  geom_point(shape=1)+
  geom_point(data=ggdat.highlight,aes(x=x,y=y),color="red")+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  xlab("Filtration rate (km/m/hr)")+
  ylab("Pellet Moisture (%)")

cor(filt.rate,moisture,method = "pearson")
cor(filt.rate,moisture,method = "spearman")
cor(filt.rate,moisture,method = "kendall")
StuartTauC(filt.rate,moisture)

cor(filt.rate2,moisture2,method = "pearson")
cor(filt.rate2,moisture2,method = "spearman")
cor(filt.rate2,moisture2,method = "kendall")
StuartTauC(filt2.rate,moisture2)





