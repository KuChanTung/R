# Example 10-12: Norml MLE Calculation

library(stats4)
x.normal = rnorm(200,66,10)	# �q N(66,10) ���� 200 �ӭ�
mLogL = function(mu,sigma) 
{
   n = 200
   x = x.normal
   n*log(sigma)+0.5*sum(((x-mu)/sigma)^2)
} 
 
mle.result = mle(minuslog= mLogL, 
		start=list(mu=mean(x.normal),sigma=sd(x.normal)))
summary(mle.result)


