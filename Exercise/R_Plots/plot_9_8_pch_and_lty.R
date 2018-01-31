# 圖 9-8 pch 與 lty 展示圖

plot(1:10,xlim=c(0,11),ylim=c(0,11),type="n",
xaxt="n",yaxt="n",xlab="",ylab="")

for(j in seq(10,1,-1))
{
  i = 10 - j + 1
  lines(c(1.5,4.5),c(j,j),lty=i)
  text(1,j,paste(i))
  points(5,j,pch=i,cex=2)
}

for(j in seq(20,11,-1))
{
  i = 20 - j + 11
  j2 = j - 10
  if (i == 0) next
  lines(c(6.5,9.5),c(j2,j2),lty=i)
  text(6,j2,paste(i))
  points(10,j2,pch=i,cex=2)
}

