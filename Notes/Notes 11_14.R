p.values<-rep(NA,10000)

for (i in 1:10000){
  random.observations<-rnorm(100,0,1)
  curr.p.value<t.test(x=random.observations,
                      alternative = "two.sided",
                      mu = 0)$p.value
  p.values[i]=curr.p.value
}

length(which(p.values<0/05))
length(which(p.values<0.05))/10000

b.adjusted<-p.adjust(p=p.values,method="bonferroni")
summary(b.adjusted)
length(which(b.adjusted<0.05))
length(which(b.adjusted<0.05))/10000

bh.adjusted<-p.adjust(p=p.values,method="BH")
summary(bh.adjusted)
length(which(bh.adjusted<0.05))
length(which(bh.adjusted<0.05))/10000
#bonferoni is more conservative - it takes a more extreme observations to get a smaller p value