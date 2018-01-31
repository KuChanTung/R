#############################################################################
# 畫出 圖 9-34 : plotmath 常用數學公式對照圖
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
#
# 請將圖形視窗調成較大的 size
############################################################################# 

library(tutoR)    # 需用到 tutoR 套件的 eval.string 函數

m = c(
"x %+-% y", "x %/% y","x %*% y", "x %.% y","x[i]",
"x^2","x^(y + z)","sqrt(x)","sqrt(x, y)","x != y",
"x <= y","x >= y","x %=~% y","x %==% y","x %prop% y",
"plain(x)","underline(x)","list(x, y, z)","hat(x)",
"tilde(x)","dot(x)","bar(xy)","widehat(xy)","widetilde(xy)",
"infinity","partialdiff","32*degree","frac(x, y)","atop(x, y)",
"sum(x[i], i==1, n)","prod(plain(P)(X==x), x)",
"integral(f(x)*dx, a, b)","union(A[i], i==1, n)",
"intersect(A[i], i==1, n)","lim(f(x), x %->% 0)",
"min(g(x), x > 0)","group('|',x,'|')"
)

plot(0:6,xlim=c(0,6.5),ylim=c(0.7,6),xlab="",ylab="",type="n",axes = T)
k = 0
nr = 10
nc = 5
jump = 0
k = k+jump

for(i in 1:nc)
{
   c0 = 0.5
   for (j in 1:nr)
   {
      c0 = c0 + 0.5
      c = 6.5 - c0
      k = k + 1
	# xpos = i
      mt = m[k]
	mm = as.name(mt)

      if (i <= 2)
         ms = paste("text(i, c,expression(",mm,"))")      
      else if (i > 2 & i <= 3)
         ms = paste("text(i+0.6, c,expression(",mm,"))")      
      else
         ms = paste("text(i+1.8, c,expression(",mm,"))")      

      if (i <= 2)
      text(i-0.5,c,mt)
      else if ( i > 2 & i <= 3)
      text(i+0.6-0.7,c,mt)
      else
      text(i+1.5-0.9,c,mt)

      eval.string(ms)
      
   }
}
