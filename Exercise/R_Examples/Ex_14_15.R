# Example 14-15 : 信賴區間的意義

mu = 165					# 模擬所需的μ = 165 (公分)
sigma = 5; n = 50				# 母體標準差設為 5 (公分）

x0 = c(160,170)
y0 = c(0,105)
plot(x0,y0,type="n")

abline(v=mu,lty=2)				

for(i in 1:100)				# 100 個迴圈
{
  x = rnorm(n,mu,sigma)			# 每個迴圈產生一組樣本,n = 50
  # 計算依據 t 分配公式算出 μ的 95% 信賴區間值 c1, c2
  width = qt(0.975,n-1)*sd(x)/sqrt(n)
  c1 = mean(x) - width
  c2 = mean(x) + width

  if ( mu >= c1 && mu <= c2 ){
       lty = 2; lwd = 1			# 若區間包含μ：普通線寬的虛線
  } else {
       lty = 1; lwd = 2			# 不含μ：2 倍線寬的實線
  }
  # 使用線段連接 c1,c2，畫出一個信賴區間水平線
  lines(c(c1,c2),c(i,i),lty=lty,lwd=lwd)
}


