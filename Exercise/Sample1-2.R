y=rnorm(100,5,10)
Q1=quantile(y,0.25)
Q3=quantile(y,0.75)

stat.names=c("最小值","Q1","平均值","中位數","Q3","最大值")
s=c(min(y),Q1,mean(y),median(y),Q3,max(y))
names(s)=stat.names
print(s)

hist(y,prob=TRUE)
lines(density(y),lty=2)
curve(dnorm(x,5,10),min(y),max(y),n=100,add = TRUE)