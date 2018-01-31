# Example 12-11: Poisson approximation to Binomail

n = 20
p = 0.05
lambda = n*p
x = matrix(0,20,4)
for (i in 0:20)
{
  pb = pbinom(i,n,p)
  pp = ppois(i,lambda)
  pdiff = abs(pb - pp)
  x[i, ] = c(i,pb,pp,pdiff)
}
colnames(x) = c("x","Binomial 機率","Poisson 機率","差異值")
x

