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
####  Plot the data
##########################################################################
##########################################################################
library(ggplot2)
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_smooth(method = "lm")+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  theme_bw()

ggplot(data=ggdat,aes(x=treatment,y=y))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="white",
               width=0.1)+
  geom_smooth(method = "lm")+
  xlab("Treatment")+
  ylab("Donation Total")+
  theme_bw()

ggplot(data=ggdat,aes(x=treatment,y=y))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="white",
               width=0.1)+
  geom_smooth(method = "lm")+
  xlab("Treatment")+
  ylab("Donation Total")+
  ylim(0,1)+
  theme_bw()



library(RVAideMemoire)
mood.medtest(DonationTot~Cond,data=dat)
#no difference in condition

summary(aov(DonationTot~Cond,data=dat))
#no difference in condition

##########################################################################
##########################################################################
####  Fit the Model
##########################################################################
##########################################################################
mod<-lm(DonationTot~SubSES*Cond,data=dat)
summary(mod)

########################################
####  Plot the model
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
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_line(data=ggdat.mod.group,aes(x=sses,y=y,color=Cond))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  theme_bw()

########################################
####marginal effects of subjective status
########################################
library(margins)
library(ggeffects)

#overall effects
(marginal.SES<-ggeffect(mod,"SubSES"))
plot(marginal.SES)
margins(mod) #is the slope of this line
summary(margins(mod))  #these can be interpreted as if the interaction weren't there

#overall effects
(marginal.Cond<-ggeffect(mod,"Cond"))
plot(marginal.Cond)
margins(mod) #is the slope of this line
summary(margins(mod))  #these can be interpreted as if the interaction weren't there

#effects by group
(marginal.SES<-ggeffect(mod,c("SubSES","Cond")))
plot(marginal.SES)
#marginal effects of subjective status by treatment group
margins(mod,variables="SubSES",
        at=list(Cond=c("Group of People","People in a Group")))

########################################
####marginal means of subjective status
########################################
library(emmeans)
emmeans(mod,"SubSES")
emmeans(mod,c("SubSES","Cond"))
emmeans(mod,c("SubSES","Cond"),
        at = list(SubSES = c(round(mean(dat$SubSES)-sd(dat$SubSES),2),
                             round(mean(dat$SubSES),2),
                             round(mean(dat$SubSES)+sd(dat$SubSES),2))))

pairs(emmeans(mod,c("SubSES","Cond"),
              at = list(SubSES = c(round(mean(dat$SubSES)-sd(dat$SubSES),2),
                                   round(mean(dat$SubSES),2),
                                   round(mean(dat$SubSES)+sd(dat$SubSES),2)))))

contrast(emmeans(mod,c("SubSES","Cond"),
                 at = list(SubSES = c(round(mean(dat$SubSES)-sd(dat$SubSES),2),
                                      round(mean(dat$SubSES),2),
                                      round(mean(dat$SubSES)+sd(dat$SubSES),2)))),
         list(lowerSES = c(1, 0, 0, -1, 0, 0),
              meanSES  = c(0, 1, 0, 0, -1, 0),
              upperSES = c(0, 0, 1, 0, 0, -1))) #group-people

plot(emmeans(mod,c("SubSES","Cond"),
             at = list(SubSES = c(round(mean(dat$SubSES)-sd(dat$SubSES),2),
                                  round(mean(dat$SubSES),2),
                                  round(mean(dat$SubSES)+sd(dat$SubSES),2)))),compairsons=TRUE)

plot(emmeans(mod,~SubSES|Cond,
             at = list(SubSES = c(round(mean(dat$SubSES)-sd(dat$SubSES),2),
                                  round(mean(dat$SubSES),2),
                                  round(mean(dat$SubSES)+sd(dat$SubSES),2)))),compairsons=TRUE)


##########################################################################
##########################################################################
####  1. Valid Model
##########################################################################
##########################################################################
# Since previous research has shown this framing (not
# related to hurricanes) but to how pain of people in 
# groups versus how groups of people experience pain
# this seems to be a valid extension

##########################################################################
##########################################################################
####  2. Linearity of the relationship
##########################################################################
##########################################################################
########################################
####  Plot the data
########################################
ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_smooth(method = "loess")+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  theme_bw()

#we see there are a few outliers that affect the otherwise 
#linear trend

########################################
####  Zooming In
########################################
#If we zoom in we can see that the association is VERY weak
cor(dat$DonationTot,dat$SubSES,method="kendall")
cor(dat$DonationTot,dat$SubSES,method="pearson")

ggdat<-data.frame(y=dat$DonationTot,
                  x=dat$SubSES,
                  treatment=dat$Cond)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(aes(color=treatment))+
  geom_smooth(method = "lm",se=FALSE)+
  ylim(c(0,1))+
  xlab("Subjective Socioeconomic Status")+
  ylab("Donation Total")+
  labs(color="")+
  theme_bw()
