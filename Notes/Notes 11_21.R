x<-rnorm(n=100000000, mean=0,sd=1)
t.test(x, mu=0, alternative="two.sided")
#still uncertainty with big sample size