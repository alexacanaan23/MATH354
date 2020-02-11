#################################################################################
####Japan Sludge#################################################################
#################################################################################
filt.rate<-c(125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,
             159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
moisture<-c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,
            79.9,79,76.7,78.2,79.5,78.1,81.5,77,79,78.6)
cor(filt.rate,moisture,method="pearson")
cor(sample(filt.rate,size=20,replace=FALSE),
    sample(moisture,size=20,replace=FALSE))

x<-rnorm(1000,0,1)
y<-rnorm(1000,5,2)
dat<-data.frame(obs=c(x,y),
                groups=c(rep("x",1000),rep("y",1000)))
t.test(x=x,y=y, conf.level=.95)
ind<-sample(1:2000,size=1000,replace=F)
mean(dat$obs[ind])-mean(dat$obs[-ind])
mean(x)-mean(y)



ggdat<-data.frame(filt.rate=filt.rate,moisture=moisture)
ggplot(ggdat, aes(x=filt.rate, y=moisture)) +
  geom_point(shape=1)+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  xlab("Filtration rate (km/m/hr)")+
  ylab("Pellet Moisture (%)")



cor(filt.rate,moisture,method = "pearson")
cor(filt.rate,moisture,method = "spearman")
cor(filt.rate,moisture,method = "kendall")
#################################################################################
####CA School data###############################################################
#################################################################################
library("Ecdat")
data(Caschool)

ggdat<-data.frame(mealpct=Caschool$mealpct,mathscr=Caschool$mathscr)
ggplot(ggdat, aes(x=mealpct, y=mathscr)) +
  geom_point(shape=1)+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  xlab("Students qualifying for reduced-price lunch (%)")+
  ylab("Average math score")

cor(Caschool$mealpct,Caschool$mathscr,method = "pearson")
cor(Caschool$mealpct,Caschool$mathscr,method = "spearman")
cor(Caschool$mealpct,Caschool$mathscr,method = "kendall")


#################################################################################
####Scatterplots###############################################################
#################################################################################
library("MASS")
generateCorrelatedData<-function(p,m=25){
  out <- as.data.frame(mvrnorm(100, mu = c(m,m), #multivariate Normal
                               Sigma = matrix(c(1,p,p,1), ncol = 2), 
                               empirical = TRUE))
  x <- (out$V1 - min(out$V1))*25+10
  y <- (out$V2 - min(out$V2))*m+10
  data.frame(x=x,y=y,corr=rep(paste("r=",p,sep=""),length(y)))
}

ggdat<-rbind(generateCorrelatedData(-0.99),
             generateCorrelatedData(-0.70),
             generateCorrelatedData(-0.40),
             generateCorrelatedData(-0.25),
             generateCorrelatedData(0.25),
             generateCorrelatedData(0.40),
             generateCorrelatedData(0.70),
             generateCorrelatedData(0.99))

ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  facet_wrap(~ggdat$corr,ncol = 4)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

corr.plots<-list()
ms<-c(-25,-15,-10,-2,25, 15, 10, 2)
for(i in 1:length(ms)){
  ggdat<-generateCorrelatedData(p=sign(ms[i]),m=abs(ms[i]))
  corr.plots[[i]]<-ggplot(data=ggdat,aes(x=x,y=y))+
    geom_point(shape=1)+
    geom_smooth(alpha=0.25,color="black")+
    ylim(c(0,150))+
    theme_bw()
}
grid.arrange(corr.plots[[1]],corr.plots[[2]],corr.plots[[3]],corr.plots[[4]],
             corr.plots[[5]],corr.plots[[6]],corr.plots[[7]],corr.plots[[8]],ncol=4)

#################################################################################
##nonlinear relationship#########################################################
#################################################################################
library("Ecdat")
data(incomeInequality)
ggdat<-data.frame(realGDPperFamily=incomeInequality$realGDPperFamily,
                  personsPerFamily=incomeInequality$personsPerFamily)
ggplot(ggdat, aes(x=realGDPperFamily, y=personsPerFamily)) +
  geom_point(shape=1)+
  geom_smooth(alpha=0.25,color="black",method="loess")+
  theme_bw()+
  xlab("Real GDP per family")+
  ylab("Persons per family")

cor(incomeInequality$realGDPperFamily,incomeInequality$personsPerFamily,method="pearson")
cor(incomeInequality$realGDPperFamily,incomeInequality$personsPerFamily,method="spearman")
cor(incomeInequality$realGDPperFamily,incomeInequality$personsPerFamily,method="kendall")


#################################################################################
##outlier               #########################################################
#################################################################################
#################################################################################
##Effect of Outliers#############################################################
#################################################################################
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

cor(filt.rate2,moisture2,method = "pearson")
cor(filt.rate2,moisture2,method = "spearman")
cor(filt.rate2,moisture2,method = "kendall")