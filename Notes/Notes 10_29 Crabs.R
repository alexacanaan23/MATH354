library("MASS")
data(crabs)
library(dplyr)
orangeCrabs<-crabs[which(crabs$sp=="O"),] ##grabs only the data needed

hist(orangeCrabs$CW)

mean(orangeCrabs$CW)
sd(orangeCrabs$CW)

point.est<-mean(orangeCrabs$CW)
se.est<-sd(orangeCrabs$CW)/sqrt(length(orangeCrabs$CW))

Margin_of_Error<-qt(p=1-0.05/2,df=(length(orangeCrabs$CW)-1)*se.est)
Margin_of_Error

point.est+c(-1,1)*Margin_of_Error

#ORRRRRR
t.test(x=orangeCrabs$CW,conf.level = 0.95)

#tstat
(mean(orangeCrabs$CW)-34)/(sd(orangeCrabs$CW)/sqrt(length(orangeCrabs$CW)))
#t cdf at t
2*pt(q=-5.452914,df=length(orangeCrabs$CW)-1)

#ORRRRR
t.test(x=orangeCrabs$CW,alternative = "two.sided", conf.level = 0.95, mu = 34)

##########################################
library("MASS")
data(crabs)
orangeCrabs<-crabs[which(crabs$sp=="O"),] ##grabs only the data needed

n<-length(orangeCrabs$CW)
mu0<-34
x.bar<-mean(orangeCrabs$CW)
alpha<-0.05

#########################
## t-test
#########################
## Hypothesis Test
mu.xbar<-mu0
se.xbar<-sd(orangeCrabs$CW)/sqrt(n)