library(ggplot2)
library(gridExtra)
########################################################################
########################################################################
####### Polynomial Regression -- GDP example
########################################################################
########################################################################
library(Ecdat)
rgdp<-incomeInequality$realGDPperFamily
ppf<- incomeInequality$personsPerFamily
cor(rgdp,ppf)

x<-seq(50000,200000,10)

mod1<-lm(ppf ~ rgdp);
y.linear<-predict(mod1,data.frame(rgdp=x))
summary(mod1)$adj.r.squared

mod5<-lm(ppf ~ poly(rgdp,5));
y.poly5<-predict(mod5,data.frame(rgdp=x))
summary(mod5)$adj.r.squared

mod10<-lm(ppf ~ poly(rgdp,10));
y.poly10<-predict(mod10,data.frame(rgdp=x))
summary(mod10)$adj.r.squared

###plot
ggdat<-data.frame(x=rgdp,
                  y=ppf)
ggdat.models<-data.frame(x=x,
                         y.linear=y.linear,
                         y.poly5=y.poly5,
                         y.poly10=y.poly10)

ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=ggdat.models,aes(x=x,y=y.linear,color="OLS"))+
  geom_line(data=ggdat.models,aes(x=x,y=y.poly5,color="5th Degree"))+
  geom_line(data=ggdat.models,aes(x=x,y=y.poly10,color="10th Degree"))+
  theme_bw()+
  xlab("Real GDP per Capita")+
  ylab("Persons per Family")+  
  labs(color="")


########################################################################
########################################################################
####### Log Transform -- Exponential Relationship
########################################################################
########################################################################
dog.dat<-read.table("C:/Users/wcipolli/OneDrive/Teaching/2019FA-354/Lectures/Lecture 26/CleanedData1.csv",header=T,sep=",")

#remove amputation OBSERVATION 184
dog.dat<-dog.dat[-which(dog.dat$name=="Sandy"),]

#PUPPIES
dat.pup<-dog.dat[which(dog.dat$age_class=="Puppy"),]
dat.pup$source<-droplevels(dat.pup$source)
dat.pup$age<-dat.pup$age *12 ##change it to month

#OLDER DOGS
dat.old<-dog.dat[which(dog.dat$age_class=="Old"),]

###For puppies
glycolytic<-dat.pup[,c(3,5,6,9,10,12,26)]
glycolytic<-glycolytic[complete.cases(glycolytic),]
glycolytic.fullp<-lm(glycolytic ~ age*weight+.,dat=glycolytic)
summary(glycolytic.fullp)
par(mfrow=c(2,2))
plot(glycolytic.fullp)

glycolytic.logp<-lm(log(glycolytic) ~ age*weight+.,dat=glycolytic)
summary(glycolytic.logp)
par(mfrow=c(2,2))
plot(glycolytic.logp)     

par(mfrow=c(1,1))
library(MASS)
bc.transform<-boxcox(glycolytic.fullp)
(lambda <- bc.transform$x[which.max(bc.transform$y)])

########################################################################
########################################################################
####### BoxCox transformation
########################################################################
########################################################################
##########################################################################
##########################################################################
####  Load the data
##########################################################################
##########################################################################
dat<-read.table("C:/Users/wcipolli/OneDrive/Teaching/2019FA-354/Lectures/Lecture 25/GroupFramingData.csv",
                header=T,sep=",")
head(dat)
dat<-dat[-74,] #Remove missing value

#make the test condition a factor
dat$Cond[which(dat$Cond==1)]="Group of People"
dat$Cond[which(dat$Cond==2)]="People in a Group"
dat$Cond = as.factor(dat$Cond)

##########################################################################
##########################################################################
####  Fit the Model
##########################################################################
##########################################################################
mod<-lm(DonationTot~SubSES*Cond,data=dat)
summary(mod)

##Is there a transformation that can help?
boxcox(mod)
#reponse needs to be >0 so we add delta =1 
bc.transform<-boxcox((DonationTot+1)~SubSES*Cond,data=dat)
(lambda <- bc.transform$x[which.max(bc.transform$y)])

dat$bcDonationTot<-(dat$DonationTot+1)^(-2)
mod.bc<-lm(bcDonationTot~SubSES*Cond,data=dat)

par(mfrow=c(2,2))
plot(mod.bc)


########################################
####  Plot the the new residuals
########################################
ggdat<-data.frame(e=mod.bc$residuals)
ggdat.gaussian<-data.frame(x=seq(-5,50,0.01),
                           f=dnorm(seq(-5,50,0.01),
                                   mean=0,               #ei should have mean zero
                                   sd=summary(mod.bc)$sigma)) #ei should have common variance  
ggplot(data=ggdat,aes(x=e))+
  geom_histogram(aes(y=..density..),bins=50,
                 fill="lightblue",color="black")+
  geom_density(aes(color="Loess Density Estimate"),size=1)+
  geom_line(data=ggdat.gaussian,aes(x=x,y=f,color="Under Gaussian Assumption"),linetype="dashed",size=1)+
  theme_bw()+
  xlab("Residual")+
  ylab("Density")

########################################
####  QQPlot of the new residuals
########################################
library("qqplotr")
ggplot(data=ggdat,aes(sample=e))+
  stat_qq_band(alpha=0.25) +
  stat_qq_line() +
  stat_qq_point() +
  theme_bw()+
  xlab("Gaussian Quantiles")+
  ylab("Sample Quantiles")

########################################
####  Tests for normality on the new residuals
########################################
##Kolmogorov Smirnov Test
ks.test(mod.bc$residuals, "pnorm",mean=0,sd=summary(mod.bc)$sigma)  #lack of normality
##Shapiro Wilkes Test
shapiro.test(mod.bc$residuals)      #lack of normality

#We weren't able to remove the lack of normality and
#even if we were, what the heck is (donation total+1)^(-2)?
#In this case we can continue down the list
#of items to check to see if it's a different issue
##### 1. Missing predictor
##### 2. Effect of outliers
##### 3. Non constant variance

########################################
####  Does the transform help?
########################################
bptest(mod.bc,studentize = TRUE) #yes, but it's not quite interpretable

dat$logDonationTot<-log(dat$DonationTot+1) #note the +1
mod.logged<-lm(logDonationTot~SubSES*Cond,data=dat)
bptest(mod.logged,studentize = TRUE) #not good enough.

########################################
####  Plot the residuals
########################################
ggdat<-data.frame(y=mod.bc$residuals)
ggplot(data=ggdat,aes(x=as.numeric(row.names(ggdat)),y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Index")+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the fitted vs residuals
########################################
ggdat<-data.frame(x=mod.bc$fitted.values,
                  y=mod.bc$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab(bquote("Fitted Values"~(hat(Y))))+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the predictor vs residuals
########################################
ggdat<-data.frame(x=mod.bc$model$SubSES,
                  y=mod.bc$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Subjective Socioeconomic Status")+
  ylab("Residual")+
  theme_bw()

#We were able to remove the non-constant variance but we
#still have to worry about 
##### 1. dependent errors
##### 2. non-gaussian errors
##########################################################################
##########################################################################
####  Robust Methods
##########################################################################
##########################################################################

########################################
####  Plot the residuals
########################################
ggdat<-data.frame(y=mod$residuals)
res1<-ggplot(data=ggdat,aes(x=as.numeric(row.names(ggdat)),y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  xlab("Index")+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

########################################
####  Plot the fitted vs residuals
########################################
ggdat<-data.frame(x=mod$fitted.values,
                  y=mod$residuals)
res2<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  xlab(bquote("Fitted Values"~(hat(Y))))+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

########################################
####  Plot the predictor vs residuals
########################################
ggdat<-data.frame(x=mod$model$SubSES,
                  y=mod$residuals)
res3<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Subjective Socioeconomic Status")+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

########################################
####  Plot the predictor vs residuals
########################################
ggdat<-data.frame(x=mod$model$Cond,
                  y=mod$residuals)
res4<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Condition")+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

grid.arrange(res1,res2,res3,res4,ncol=2)

########################################
####  Get weights
########################################
dat$absres<-abs(mod$residuals)
dat$fitted<-mod$fitted.values

mod.weight1SES<-lm(absres~SubSES,data=dat)
mod.weight2<-lm(absres~fitted,data=dat)

weight1<-1/(fitted(mod.weight1SES))^2
weight2<-1/(fitted(mod.weight2))^2

########################################
####  Plot the models (weight1SES)
########################################
mod.wls1<-lm(DonationTot~SubSES*Cond,data=dat,weights = weight1)
summary(mod.wls1)

ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod.wls1,newData),
                            Cond=newData$Cond)
g.wls1<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Weighted Least Squares Regression",
          subtitle="Using weights modeled by subjective SES")+
  theme_bw()


########################################
####  Plot the models (fitted)
########################################
mod.wls2<-lm(DonationTot~SubSES*Cond,data=dat,weights = weight2)
summary(mod.wls2)

ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod.wls2,newData),
                            Cond=newData$Cond)
g.wls2<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Weighted Least Squares Regression",
          subtitle="Using weights modeled by fitted values")+
  theme_bw()

grid.arrange(g.wls1,g.wls2,ncol=1)
########################################
####  Fit Robust Models -- Iterated Reweighted Least Squares
########################################
library(MASS)

####HUBER
huberWeight<-function(e,k=1.345){
  w= (1/abs(e))*(abs(e)>k) + (abs(e)<=k)
  return(w)
}
####bisquare
bisquareWeight<-function(e,k=4.685){
  w= (1-(e^2/k^2))^2 *(abs(e)<=k)
  return(w)
}

ggdat<-data.frame(e=seq(-10,10,0.01),
                  huber=huberWeight(seq(-10,10,0.01)),
                  bisquare=bisquareWeight(seq(-10,10,0.01)))
g1<-ggplot(data=ggdat,aes(x=e,y=huber))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = -4,col="red",linetype="dashed")+
  geom_vline(xintercept = 4,col="red",linetype="dashed")+
  xlab("Residual")+
  ylab("Weight")+
  ggtitle("Huber Weighting")+
  theme_bw()
g2<-ggplot(data=ggdat,aes(x=e,y=bisquare))+
  geom_line()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = -4,col="red",linetype="dashed")+
  geom_vline(xintercept = 4,col="red",linetype="dashed")+
  xlab("Residual")+
  ylab("Weight")+
  ggtitle("Bisquare Weighting")+
  theme_bw()

library(gridExtra)
grid.arrange(g1,g2,ncol=2)

####################################
## Huber
####################################
mod.huber<-rlm(DonationTot~SubSES*Cond,data=dat,psi=psi.huber)
summary(mod.huber)
library(sfsmisc)
f.robftest(mod.huber,var="SubSES")
f.robftest(mod.huber,var="CondPeople in a Group")
f.robftest(mod.huber,var="SubSES:CondPeople in a Group")

####################################
## Bisquare
####################################
mod.bisquare<-rlm(DonationTot~SubSES*Cond,data=dat,psi=psi.bisquare)
summary(mod.bisquare)
f.robftest(mod.bisquare,var="SubSES")
f.robftest(mod.bisquare,var="CondPeople in a Group")
f.robftest(mod.bisquare,var="SubSES:CondPeople in a Group")

########################################
#### Quantile Regression
########################################
library(quantreg)
mod.quantile<-rq(DonationTot~SubSES*Cond,data=dat)
summary(mod.quantile,se="ker")

########################################
####  Plot the models
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod,newData),
                            Cond=newData$Cond)
g.OLS<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("OLS Regression")+
  theme_bw()

########################################
####  Plot the models
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod.huber,newData),
                            Cond=newData$Cond)
g.huber<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Huber IRWLS Regression")+
  theme_bw()

########################################
####  Plot the models
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod.bisquare,newData),
                            Cond=newData$Cond)
g.bisquare<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Bisquare IRWLS Regression")+
  theme_bw()


########################################
####  Plot the models
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y=predict(mod.quantile,newData),
                            Cond=newData$Cond)
g.rq<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Quantile Regression")+
  theme_bw()

grid.arrange(g.OLS,g.huber,g.bisquare,g.rq,ncol=2)

########################################
####  Plot all of the models
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)

newData<-data.frame(SubSES=rep(seq(0,10,0.01),2),
                    Cond=c(rep("Group of People",length(seq(0,10,0.01))),
                           rep("People in a Group",length(seq(0,10,0.01)))))
ggdat.mod.group<-data.frame(sses=newData$SubSES,
                            y.ols=predict(mod,newData),
                            y.wls1=predict(mod.wls1,newData),
                            y.wls2=predict(mod.wls2,newData),
                            y.irls1=predict(mod.bisquare,newData),
                            y.irls2=predict(mod.huber,newData),
                            y.rq=predict(mod.quantile,newData),
                            Cond=newData$Cond)
all<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(shape=treatment),show.legend = F)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.ols,linetype=Cond,color="OLS"))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.wls1,linetype=Cond,color="WLS1"))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.wls2,linetype=Cond,color="WLS2"))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.irls1,linetype=Cond,color="Bisquare"))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.irls2,linetype=Cond,color="Huber"))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.rq,linetype=Cond,color="Quantile"))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ggtitle("Quantile Regression")+
  theme_bw()+
  scale_shape_manual("Condition",values = c("Group of People"=0, "People in a Group"=1))


all.zoom<-ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(shape=treatment),show.legend = F)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.ols,linetype=Cond,color="OLS"),size=1)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.wls1,linetype=Cond,color="WLS1"),size=1)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.wls2,linetype=Cond,color="WLS2"),size=1)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.irls1,linetype=Cond,color="Bisquare"),size=1)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.irls2,linetype=Cond,color="Huber"),size=1)+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y.rq,linetype=Cond,color="Quantile"),size=1)+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  ylim(c(0,1))+
  ggtitle("Quantile Regression")+
  theme_bw()+
  scale_shape_manual("Condition",values = c("Group of People"=0, "People in a Group"=1))

grid.arrange(all,all.zoom,ncol=2)
##########################################################################
##########################################################################
####  Model Selection
##########################################################################
##########################################################################
library("Rspotify")
#place your application information below.
keys <- spotifyOAuth("MATH_354",
                     "e1919c8819e04e5ab113715df7896283",
                     "f2d7d2da7b42443db72f0ab010bd6399")


songs.1<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys) #1-100
##Download features about the tracks
features.1<-data.frame()
for(i in 1:nrow(songs.1)){#for each song
  features.1<-rbind(features.1,getFeatures(songs.1$id[i],token=keys))
}

songs.2<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys,offset=100) #101-200
features.2<-data.frame()
##Download features about the tracks
for(i in 1:nrow(songs.2)){#for each song
  features.2<-rbind(features.2,getFeatures(songs.2$id[i],token=keys))
}

songs.3<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys,offset=200) #201-300
features.3<-data.frame()
##Download features about the tracks
for(i in 1:nrow(songs.3)){#for each song
  features.3<-rbind(features.3,getFeatures(songs.3$id[i],token=keys))
}

songs.4<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys,offset=300) #301-400
features.4<-data.frame()
##Download features about the tracks
for(i in 1:nrow(songs.4)){#for each song
  features.4<-rbind(features.4,getFeatures(songs.4$id[i],token=keys))
}

songs.5<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys,offset=400) #401-500
features.5<-data.frame()
##Download features about the tracks
for(i in 1:nrow(songs.5)){#for each song
  features.5<-rbind(features.5,getFeatures(songs.5$id[i],token=keys))
}

songs.6<-getPlaylistSongs("spotify","2Dx7ZFptRzQUqhqV52gNYH",token=keys,offset=500) #500-543
features.6<-data.frame()
##Download features about the tracks
for(i in 1:nrow(songs.6)){#for each song
  features.6<-rbind(features.6,getFeatures(songs.6$id[i],token=keys))
}

features<-rbind(features.1,features.2,features.3,features.4,features.5,features.6)
songs<-rbind(songs.1,songs.2,songs.3,songs.4,songs.5,songs.6)
##Data frame for saving features
df<-data.frame(artist=songs$artist,track=songs$tracks,album=songs$album,
               danceability=features$danceability,
               energy=features$energy,
               key=features$key,
               loudness=features$loudness,
               mode=features$mode,
               speechiness=features$speechiness,
               acousticness=features$acousticness,
               instrumentalness=features$instrumentalness,
               liveness=features$liveness,
               valence=features$valence,
               tempo=features$tempo,
               duration_ms=features$duration_ms,
               time_signature=features$time_signature
)

dance.formula<- as.formula(danceability~ (energy+key+tempo+loudness)^2+mode+speechiness+acousticness+
                             instrumentalness+liveness+valence+duration_ms+
                             time_signature)

########################################
####  Best glm: AIC
########################################
library(bestglm)
y<-df$danceability
xfactors<-model.matrix(dance.formula,data = df)[, -1]
Xy<-data.frame(xfactors,y)
best.model_AIC<-bestglm(Xy, IC="AIC")
summary(best.model_AIC$BestModel)

########################################
####  Best glm: AIC
########################################
y<-df$danceability
xfactors<-model.matrix(dance.formula,data = df)[, -1]
Xy<-data.frame(xfactors,y)
best.model_BIC<-bestglm(Xy, IC="BIC")
summary(best.model_BIC$BestModel)

########################################
####  Compare Models
########################################
library(lmtest)
best.lm.aic<-lm(danceability~energy+loudness+mode+speechiness+valence+duration_ms,data=df)
best.lm.bic<-lm(danceability~energy+mode+valence,data=df)
lrtest(best.lm.bic,best.lm.aic) #aic fit somewhat better (p=0.02058)

########################################
####  Stepwise Models
########################################
full.mod<-lm(dance.formula,data=df)
library(MASS)
stepAIC(full.mod, trace = FALSE)

stepAIC(full.mod, trace = FALSE,k=log(nrow(df))) #setting k gets BIC

#these yield the same models.

########################################
####  The lasso
########################################
library(glmnet)
y<-df$danceability
xfactors<-model.matrix(dance.formula,data = df)[, -1]
x<-as.matrix(data.frame(xfactors))
lasso.cv <- cv.glmnet(x=x,y=y,alpha = 1) #fits for many lambdas (penalties)

coef(lasso.cv, "lambda.min") #minimizes cross validation error
lasso.cv$lambda.min

coef(lasso.cv, "lambda.1se") #largest lambda so that cross validation error is
#within 1 standard error of minimum
lasso.cv$lambda.1se

plot(lasso.cv)

#human choice
lasso <- glmnet(x=x,y=y,alpha=1,lambda=exp(-4)) #smart fit (could change from run to run)
coef(lasso)

