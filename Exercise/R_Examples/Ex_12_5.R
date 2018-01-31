# Example 12-5: pdf and cdf plots

# N(0,1)
curve(dnorm(x), from = -3.5, to = 3.5,ylab="pdf",
		main="N(0,1) pdf")

devAskNewPage(ask = TRUE)

curve(pnorm(x), from = -3.5, to = 3.5,ylab="cdf",
		main="N(0,1) cdf")

devAskNewPage(ask = TRUE)

# Gamma(1,1)
g = rgamma(100,1,1)
curve(dgamma(x,1,1), min(g), max(g),xlab="x",ylab="pdf",main="Gamma(1,1) pdf")

devAskNewPage(ask = TRUE)

curve(pgamma(x,1,1), min(g), max(g),xlab="x",ylab="cdf",main="Gamma(1,1) cdf")


# Binomial(n=20, p=0.2) 的 pdf 與 cdf 圖形(圖 12-3)：

devAskNewPage(ask = TRUE)

pdf = dbinom(0:20,20,0.2)
plot(0:20,pdf,type="h",main="B(20,0.2) pdf",lwd=4)

devAskNewPage(ask = TRUE)

barplot(pdf,names=as.character(0:20))
cdf = pbinom(0:20,20,0.2)

devAskNewPage(ask = TRUE)
plot(0:20, cdf, type="S", xlab="x", main="B(20,0.2) cdf")

