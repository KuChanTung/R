# Example 12-6: Empirical CDF plot

x = rnorm(40) 	# º“¿¿40≠” N(0,1) ∂√º∆
Fn = ecdf(x)
Fn

summary(Fn)
Fn(2.3)
plot(Fn,main="plot(Fn)")

devAskNewPage(ask = TRUE)

plot.ecdf(x,main="plot.ecdf(x)")

