# Example 12-8: CLT for Binomail Distribution

oldpar = par()
par(mfrow=c(2,2))

p = 0.2
ns = c(10,20,30,50)
k = 2000
for (n in ns) 
{
  Zn = numeric(k)
  for (j in 1:k) 
  {
	 Xn = rbinom(1,n,p)					# Xn ~ Binom(n,p)
    Zn[j] = ( Xn/n - p )/sqrt(p*(1-p)/n)  
  }

  hist(Zn,breaks=25,main=paste("n = ",n),
		b=expression(Z[n]))
}

par(oldpar)

