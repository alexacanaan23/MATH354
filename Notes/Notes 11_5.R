n.ia<-439
n.sc<-731
n.nh<-1266
  
x.nh<-round(0.15*n.nh,0)
x.sc<-round(0.30*n.sc,0)
x.ia<-round(.17*n.ia,0)

library(binom)
binom.confint(x=x.ia, n=n.ia, conf.level = 0.95, methods = "wilson")
binom.confint(x=x.sc, n=n.sc, conf.level = 0.95, methods = "wilson")
binom.confint(x=x.nh, n=n.nh, conf.level = 0.95, methods = "wilson")

prop.test(x=c(x.nh,x.sc), n=c(n.nh,n.sc), conf.level = 0.95)
prop.test(x=c(x.nh,x.sc), n=c(n.nh,n.sc), conf.level = 0.95,
          p=0,
          alternative = "less")
prop.test(x=c(x.nh,x.ia), n=c(n.nh,n.ia), conf.level = 0.95)

library(MASS)
data("uis")
hist(uis$AGE)
mean(uis$AGE[which(uis$SITE=="0")])
mean(uis$AGE[which(uis$SITE=="1")])
median(uis$AGE[which(uis$SITE=="0")])
median(uis$AGE[which(uis$SITE=="1")])
boxplot(uis$AGE~uis$SITE)

t.test(x=uis$AGE[which(uis$SITE=="0")],
       y=uis$AGE[which(uis$SITE=="1")],
       conf.level = 0.95,
       var.equal = T)
