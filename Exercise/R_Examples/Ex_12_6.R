# Example 12-6: Empirical CDF plot

x = rnorm(40) 	# ����40�� N(0,1) �ü�
Fn = ecdf(x)
Fn

summary(Fn)
Fn(2.3)
plot(Fn,main="plot(Fn)")

devAskNewPage(ask = TRUE)

plot.ecdf(x,main="plot.ecdf(x)")

