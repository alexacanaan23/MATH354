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
  ylim(0,1)+
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
cor(dat$DonationTot,dat$SubSES,method="pearson")
cor(dat$DonationTot,dat$SubSES,method="spearman")
cor(dat$DonationTot,dat$SubSES,method="kendall")

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

##########################################################################
##########################################################################
####  3. Errors are Independent
##########################################################################
##########################################################################
########################################
####  Plot the residuals
########################################
ggdat<-data.frame(y=mod$residuals)
ggplot(data=ggdat,aes(x=as.numeric(row.names(ggdat)),y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Index")+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the fitted vs residuals
########################################
ggdat<-data.frame(x=mod$fitted.values,
                  y=mod$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab(bquote("Fitted Values"~(hat(Y))))+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the residuals vs residuals lag 1
########################################
ggdat<-data.frame(x=mod$residuals[1:(length(mod$residuals)-1)],
                  y=mod$residuals[2:length(mod$residuals)])
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab(bquote(e[i-1]))+
  ylab(bquote(e[i]))+
  theme_bw()

########################################
####  Plot the residuals vs residuals lag 2
########################################
ggdat<-data.frame(x=mod$residuals[1:(length(mod$residuals)-2)],
                  y=mod$residuals[3:length(mod$residuals)])
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab(bquote(e[i-2]))+
  ylab(bquote(e[i]))+
  theme_bw()

########################################
####  tests
########################################
library("lmtest")
####################
# Durbin Watson
####################
res<-mod$residuals
(numerator<- sum((res[2:(length(res))]-res[1:(length(res)-1)])^2))
(denominator<- sum(res^2))
(DW<-numerator/denominator)

#the p-value calculation is complicated
#so we'll let R do it:
dwtest(mod)

####################
# Breusch-Godfrey Test
####################
## Get the residuals
res <- mod$residuals
n<-length(res)
####Testing order=p=1 with one predictor
p=1 #order of lag = p = 1
k=4 #four coefficients in the original model

bg.dat<-NULL
bg.dat$res<-res[2:n]
bg.dat$res1<-res[1:(n-1)]
bg.dat$SubSES<-dat$SubSES[2:n]
bg.dat$Cond<-dat$Cond[2:n]

bgmod<-lm(res~SubSES*Cond+res1,data=bg.dat)
r2<-summary(bgmod)$r.squared
(LM<-(n*sum(bgmod$fitted^2))/sum(res^2))
1-pchisq(q=LM,df=p)

(Fstar= ((n-k-p)/p)*((sum(res^2) - sum(bgmod$residuals^2))/sum(bgmod$residuals^2)))
1-pf(q = Fstar,p,n-k-p)

bgtest(mod,order = 1,type="F")
bgtest(mod,order = 2,type="F")
bgtest(mod,order = 1,type="Chisq")
bgtest(mod,order = 2,type="Chisq")

##Here we see that the errors appear to be related
##to the fitted value through liekly through
##the outliers. In this case we can continue down the
##list of items to check to see if it's a different 
##issue (likely outlying observations)
##### 1. Missing predictor
##### 2. Effect of outliers
##### 3. Non constant variance
##### 4. Non Gaussian Errors

##########################################################################
##########################################################################
####  4. Errors are Normally Distributed
##########################################################################
##########################################################################
########################################
####  Plot the residuals
########################################
ggdat<-data.frame(e=mod$residuals)
ggdat.gaussian<-data.frame(x=seq(-5,50,0.01),
                           f=dnorm(seq(-5,50,0.01),
                                   mean=0,               #ei should have mean zero
                                   sd=summary(mod)$sigma)) #ei should have common variance  
ggplot(data=ggdat,aes(x=e))+
  geom_histogram(aes(y=..density..),bins=50,
                 fill="lightblue",color="black")+
  geom_density(aes(color="Loess Density Estimate"),size=1)+
  geom_line(data=ggdat.gaussian,aes(x=x,y=f,color="Under Gaussian Assumption"),linetype="dashed",size=1)+
  theme_bw()+
  xlab("Residual")+
  ylab("Density")+
  labs(color = "")

#This is highly right skewed -- we see an issue with the normality assumption
#The mismatch is largely due to the outliers which increase the estimated
#standard deviation. Besides the outliers the distribution is kind of normal 
#looking.

########################################
####  Plot the residuals ecdf
########################################
e.cdf.func<-ecdf(mod$residuals)
e.cdf<-e.cdf.func(sort(mod$residuals))

ggdat<-data.frame(e=sort(mod$residuals),
                  e.cdf=e.cdf)
ggdat.gaussian<-data.frame(x=seq(-5,50,0.01),
                           CDF=pnorm(seq(-5,50,0.01),
                                     mean=0,               #ei should have mean zero
                                     sd=summary(mod)$sigma)) #ei should have common variance  
ggplot(data=ggdat,aes(x=e))+
  geom_step(aes(y=e.cdf,color="Empirical CDF"))+
  geom_line(data=ggdat.gaussian,aes(x=x,y=CDF,color="Under Gaussian Assumption"),linetype="dashed",size=1)+
  theme_bw()+
  xlab("Residual")+
  ylab("Cumulative Density")+
  labs(color = "")

#We see there is a big difference in the steepness of the empirical cdf
#and the gaussian cdf. This indicates that the variance in the gaussian
#assumption is larger than in the observed errors.

########################################
####  QQplot of the residuals
########################################
library("qqplotr")
ggplot(data=ggdat,aes(sample=e))+
  stat_qq_band(alpha=0.25) +
  stat_qq_line() +
  stat_qq_point() +
  theme_bw()+
  xlab("Gaussian Quantiles")+
  ylab("Sample Quantiles")

##qqplot confirms that the errors are roughly normal except for the observations
##in the tail -- the story is coming together: these outliers are causing us trouble

########################################
####  Tests for e~Gaussian(0,sigma) 
########################################
##Shapiro Wilkes Test
shapiro.test(mod$residuals)      #lack of normality
##Kolmogorov Smirnov Test
ks.test(mod$residuals, "pnorm",mean=0,sd=summary(mod)$sigma)  #lack of normality


##########################################################################
##########################################################################
####  5. Constant Variance
##########################################################################
##########################################################################
########################################
####  Plot the residuals
########################################
ggdat<-data.frame(y=mod$residuals)
ggplot(data=ggdat,aes(x=as.numeric(row.names(ggdat)),y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Index")+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the fitted vs residuals
########################################
ggdat<-data.frame(x=mod$fitted.values,
                  y=mod$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab(bquote("Fitted Values"~(hat(Y))))+
  ylab("Residual")+
  theme_bw()

########################################
####  Plot the predictor vs residuals
########################################
ggdat<-data.frame(x=mod$model$SubSES,
                  y=mod$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  xlab("Subjective Socioeconomic Status")+
  ylab("Residual")+
  theme_bw()

#Breusch Pagan Test
bptest(mod,studentize = FALSE)
#Studentized Breusch Pagan Test -- this is better and has reduced assumptions
bptest(mod,studentize = TRUE)
##this test essentially checks if our predictors are predictive 
##of the residuals.

#########################
# By hand
#########################
## Get the residuals
res<-residuals(mod)
dat$res2<-res^2

#SSE = Sum of squared error
sse <- sum(res^2)
sse

#SSR = Sum of squared regresion = explained sum of squares (ESS)
bpmod<-lm(res2~SubSES*Cond,data=dat)
yhat<-fitted(bpmod)
ybar<-mean(dat$res2)
ssr = sum((yhat-ybar)^2)
ssr

##Not studentized
(.5*ssr)/(sse/nrow(dat))^2

##Studentized
n<-nrow(dat)
v1<-var(res)
v2<-var(res^2)
lambda = (n*v2)/(2*(n-1)*v1^2)
(0.5*ssr)/(lambda*(sse/nrow(dat))^2)



##########################################################################
##########################################################################
####  6. No outliers
##########################################################################
##########################################################################
########################################
####  Plot the residuals
########################################
ggdat<-data.frame(y=mod$residuals)
ggplot(data=ggdat,aes(x=as.numeric(row.names(ggdat)),y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  xlab("Index")+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

#We can see that if I remove those points, I would have really good behavior
#of the error terms.

########################################
####  Plot the fitted vs residuals
########################################
ggdat<-data.frame(x=mod$fitted.values,
                  y=mod$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
  geom_point(shape=1,aes(color= abs(y)>4))+
  geom_hline(yintercept = -4,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 0,color="red",linetype="dashed",size=1)+
  geom_hline(yintercept = 4,color="red",linetype="dashed",size=1)+
  xlab(bquote("Fitted Values"~(hat(Y))))+
  ylab("Residual")+
  scale_color_manual("",values = c("black", "red"),
                     labels = c("Not Outlying", "Outlying")) + 
  theme_bw()

#We can see that the fitted values don't necessarily relate
#to which points are outlying

########################################
####  Plot the predictor vs residuals
########################################
ggdat<-data.frame(x=mod$model$SubSES,
                  y=mod$residuals)
ggplot(data=ggdat,aes(x=x,y=y))+
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
#We can see that the subjective ses values don't necessarily relate
#to which points are outlying -- that is it's not those that think
#they're well off offering to donate the most  though the outlying
#points somewhat show that

## I can remove the outliers and refit the data, but these values don't
## appear to be misentered -- it's completely plausible that participants
## wanted to donate up to $50 dollars

##########################################################################
##########################################################################
####  7. Missing Predictor
##########################################################################
##########################################################################
mod.subset<-lm(DonationTot~Cond,data=dat)

ggdat<-data.frame(ses=dat$SubSES,
                  e=mod.subset$residuals)

ggplot(data=ggdat,aes(x=ses,y=e))+
  geom_point()+
  geom_smooth(method="loess")+
  theme_bw()+
  xlab("Subjective Socioeconomic Status")+
  ylab("Residual")

ggplot(data=ggdat,aes(x=ses,y=e))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  xlab("Subjective Socioeconomic Status")+
  ylab("Residual")

lrtest(mod.subset,mod)
waldtest(mod.subset,mod)

##########################################################################
##########################################################################
####  8. No Multicollinearity
##########################################################################
##########################################################################
library(car)
vif(mod)
mod.vif<-lm(DonationTot~Cond+SubSES,data=dat)
vif(mod.vif) #none are greater than 10