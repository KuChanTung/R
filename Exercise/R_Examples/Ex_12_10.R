# Example 12-10: Normal approximation to Binomial distribution

n = 150; p = 0.05
mu = n*p
sigma = sqrt(n*p*(1-p))
x = 0:20
pb = pbinom(x,,p)
pn = pnorm(x,mu,sigma)
pdiff = abs(pb - pn)
mytable = cbind(x,pb,pn,pdiff)
colnames(mytable) = c("x","Binomial 機率","Normal 機率","差異值")
mytable


