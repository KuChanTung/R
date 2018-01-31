# Example 12-9: Chi-Square(1) CLT

oldpar = par()
par(mfrow=c(2,2))

library(ConvergenceConcepts)
rand1 = function(n) {
        (cumsum(rchisq(n, df = 1)) - (1:n))/sqrt(2 * (1:n))
}
data = generate(randomgen = rand1, nmax=2000, M=500)$data
tinf = -4 ; tsup = 4
for (nn in c(1,5,10,100))
{
	plot.ecdf(ecdf(data[, nn]), do.points = FALSE, 
			xlim = c(tinf, tsup), col.h = "red", 
			xlab = "", main = paste("n = ",nn))
	curve(pnorm, xlim = c(tinf, tsup), lty=2, xlab="",
			ylab="",main= "", add = TRUE)
}

devAskNewPage(ask = TRUE)

par(mfrow=c(1,1))
law.plot3d(data,pnorm)

par(oldpar)