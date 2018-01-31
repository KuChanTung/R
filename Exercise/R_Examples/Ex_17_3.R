# Example 17-3: p and np chart

library(qcc)

x = c(6,4,8,5,4,3,9,7,6,6,3,7,6,5,6,3,2,5,3,0,7,3,4,6,7)
x.p = qcc(data=x,sizes=50,type="p")
x.p$center
x.p$limits
x.np = qcc(data=x,sizes=50,type="np")
x.np$center
x.np$limits
