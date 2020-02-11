install.packages("binom")
library("binom")
#binom.confint()
binom.test(x=36, n=10000,conf.level = .95, p=0.0033, alternative="greater")
