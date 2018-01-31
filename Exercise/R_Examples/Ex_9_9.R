# Example 9-9: use polygon to make shadow area of N(0,1) and 
#   Chi-Square(20) density plot

make.shadow = function(xStart,xEnd,xIncr,func=dnorm,...)
{
  middle = seq(xStart,xEnd,by=xIncr)
  x0 = c(xStart,middle,xEnd)
  y0 = c(0,func(middle,...),0)
  return(list(x=x0,y=y0))
}

s = make.shadow(-3,1.96,0.05,func=dnorm)		# N(0,1)
curve(dnorm(x),-3,3)
abline(h=0)
polygon(s$x,s$y,density=40,angle=45)

s = make.shadow(0,25,0.05,func=dchisq,20)	# 卡方分配(df=20)
curve(dchisq(x,20),0,80)
abline(h=0)
polygon(s$x,s$y,density=40,angle=45)

