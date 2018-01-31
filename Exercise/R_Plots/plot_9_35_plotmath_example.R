#############################################################################
# 畫出 圖 9-35 : plotmath 示範範例
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
#
# 請將圖形視窗調成較大的 size
############################################################################# 

plot(c(-pi, -pi/2, 0, pi/2, pi),1:5,type="n",,xaxt="n",
        main = expression(paste(plain(sin) * phi, "  與  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi), 
        xlab = expression(paste("Phase Angle ", phi)) )
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
		labels = expression(-pi, -pi/2, 0, pi/2, pi))
for (i in 1:5)
{
  text(-2.5,i, substitute(list(xi,eta) == group("(",list(x,y),
			")"), list(x=i, y=i+1)))
}
text(-1.7,5, expression("一階微分" == {f * minute}(x)), adj=0)
text(-1.7,4, expression("二階微分" == {f * second}(x)), adj=0)
text(-1.7, 3,pos=4,expression(hat(beta) == 
				(X^t * X)^{-1} * X^t * y))
text(-1.7, 2,pos=4, expression(bar(x) == sum(frac(x[i], n), 
				i==1, n)))
text(-1.7, 1.2, pos=4,expression(paste(frac(1, 
					sigma*sqrt(2*pi)), " ",
          plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})), cex = 1.2)
text(0.5, 4.6, pos=4,expression(prod(plain(P)(X==k), k=1,n)))
text(0.5, 4, pos=4,expression(integral(f(x)*dx, a, b)))
text(0.5, 3, pos=4,expression(union(A[i], i==1, n)))
text(0.5, 2, pos=4,expression(intersect(A[i], i==1, n)))
text(0.5, 1, pos=4,expression(lim(f(x), x %->% 0)))
text(2, 4.5, pos=4,expression(min(f(x), x > 0)))
text(1, 3.5, pos=4,expression(Y 
		== beta[0] + list(beta[1]*X[1],...,beta[p-1]*X[p-1])))
text(1.5, 2.5,pos=4,expression(S^2 
				== sqrt(frac(sum((X[i]-bar(x))^2),n-1))))

