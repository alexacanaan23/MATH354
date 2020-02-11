install.packages("quantreg",repos = "http://cloud.r-project.org/")
library("quantreg")
data("uis")
plot(x=uis$RACE, y=uis$BECK,  xlab="Race", ylab="Beck Depression Score")

boxplot(uis$BECK,
        main = "Title",
        xlab = "X Label",
        ylab = "Y Label",
        col= "lightblue",
        ylab= "Frequency of Beck Depression Scores")
points(jitter(rep(1,length(uis$RACE)),amount =.1),uis$BECK)

boxplot(BECK~RACE, data=uis, #beck depression score by race
        main = "Title",
        xlab = "X Label",
        ylab = "Y Label",
        col = "lightblue")

hist(x=uis$BECK,
     main= "Title",
     xlab = "X Label",
     ylab = "Y Label",
     probability = TRUE,#probability, not density
     col="lightblue")
lines(density(uis$BECK))
abline(h=0)

barplot(table(uis$RACE), #frequency
        main = "Title",
        xlab= "X Label",
        ylab= "Y Label",
        col= "lightblue")
abline(h=0)

barplot(prop.table(table(uis$RACE)), #relative frequency
        main = "Title",
        xlab= "X Label",
        ylab= "Y Label",
        col= "lightblue")
abline(h=0)
#################
install.packages("ggplot2")
library(ggplot2)

ggdat<-data.frame(table(uis$BECK))
colnames(ggdat)=c("Beck_Depression_Score", "Count")
ggplot(data=ggdat, aes(x=Beck_Depression_Score, y=Count)) +
  geom_bar(stat="identity",
           color = "black",
           fill="lightblue") +
  xlab("Beck Depression Score") +
  ylab("Frequency") +
  ggtitle("Frequency of Beck Depression Score")+
  geom_hline(yintercept = 0) +
  theme_bw()

ggdat<-data.frame(prop.table(table(uis$BECK)))
colnames(ggdat)=c("Beck_Depression_Score", "Count")
ggplot(data=ggdat, aes(x=Beck_Depression_Score, y=Count)) +
  geom_bar(stat="identity",
           color = "black",
           fill="lightblue") +
  xlab("Beck Depression Score") +
  ylab("Frequency") +
  ggtitle("Frequency of Beck Depression Score")+
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(data = uis, aes(x=BECK)) +
  geom_histogram(bins=10,
                 fill = "lightblue", color = "black") +
  xlab("Frequency of X") +
  ylab("Frequency of Y") +
  ggtitle("Title") +
  theme_bw() +
  geom_hline(yintercept = 0)

ggplot(data=uis, aes(x=BECK)) +
  geom_histogram(aes(y=..density..),
                 binwidth = density(uis$BECK)$bw,
                 fill = "lightblue", color = "black") +
  geom_density(fill="red", alpha = 0.2) +
  theme_bw() +
  geom_hline(yintercept = 0)

ggplot(data=uis, aes(x=BECK)) +
  geom_histogram(aes(y=..density..),
                 binwidth = density(uis$BECK)$bw,
                 fill = "lightblue", color = "black") +
  geom_density(fill="red", alpha = 0.2) +
  theme_bw() +
  geom_hline(yintercept = 0)

ggplot(data=uis, aes(x='',y=BECK))+
  geom_boxplot(fill="lightblue")+
  xlab("") +
  ylab("Frequencies of Beck Depression Scores") +
  ggtitle("GGtitle") +
  theme_bw()

ggplot(data=uis, aes(x='',y=BECK))+
  geom_boxplot(fill="lightblue")+
  xlab("") +
  ylab("Frequencies of Beck Depression Scores") +
  ggtitle("GGtitle") +
  theme_bw() +
  geom_jitter(position = position_jitter(.2))

ggplot(data=uis, aes(x="", y=BECK)) +
  geom_violin(fill="pink",
              trim = FALSE,
              draw_quantiles = c(.25,.5,.75),
              alpha = 0.5,
              show.legend = FALSE) +
  geom_boxplot(fill="white") +
  xlab("xlab") +
  ylab("Y LAB") +
  ggtitle("GGTITLE") +
  theme_bw()
#################
x<-seq(0,100,0.01)
f<-dchisq(x, df=14)
plot(x,f,type="l")
abline(h=0)

Fx<-pchisq(x,df=14)
plot(x, Fx, type="l")
abline(h=0)

#P(X<4)
pchisq(q=4,df=14)
1-pchisq(q=4,df=14)
pchisq(q=4,df=14,lower.tail = FALSE)

#################
x.chisq<-rchisq(n=100, df=12.5)
hist(x.chisq)
#MLE
chisq.ll<-function(x.data,theta,neg=FALSE) {
  v<-theta
  ll<-sum(dchisq(x=x.data,df=v, log=TRUE))
  ifelse(!neg,ll,-ll)
}
optim(par=mean(x.chisq),
      fn = chisq.ll, #function to minimize
      x.data=x.chisq, #data
      neg=TRUE,
      method="Brent",
      lower = min(x.chisq),
      upper = max(x.chisq)) #I need the neg ll

#MOM
#use moments - E(X) uncentered moment, var(X) centered moment
x<seq(0,50,0.01)
f1<-dchisq(x,df=1)
f2<-dchisq(x,df=5)
f3<-dchisq(x,df=10)
f4<-dchisq(x,df=15)
f5<-dchisq(x,df=20)

hist(x.chisq,probability=TRUE)
plot(x,f5,type="l",col="black")
lines(x,f1, type ="l",col="red")
lines(x,f2, type= "l",col="orange")
lines(x,f3, type= "l",col="yellow")
lines(x,f4, type= "l",col="green")
lines(x,f5, type= "l",col="blue")


hist(x.chisq,probability=TRUE)
lines(x,dchisq(x,df=12.5), lwd= 'black')
lines(x,f1,col="red")
lines(x,f2,col="orange")
lines(x,f3,col="yellow")
lines(x,f4,col="green")
lines(x,f5,col="blue")

g<-function(x.data,theta){
  v<-theta
  EX<- v
  EX-mean(x.data)
}

library(nleqslv)
nleqslv(x = mean(x.chisq), #first guess
        fn = g,
        x.data = x.chisq)

############
x.pois<-rpois(1000, 10)
sd(x.pois)
dpois(5,10,log=FALSE)
ppois(2,10,log=FALSE)

x.binom<-rbinom(1000,11,.15)
sd(x.binom)
dbinom(5,11,.15,log=FALSE)
pbinom(2,11,.15,lower.tail=TRUE,log=FALSE)

x.norm<-rnorm(1000,1050,60^2)
pnorm(970,1050,60,log=FALSE)
pnorm(1140,1050,60,lower.tail=FALSE, log=FALSE)

x.unif<-runif(1000,850,1250)

############