# Example 9-10: legend example
#
# 比較兩種教學方法的成績 (圖 9-30)：
#
x = sample(1:100,20)		# 隨機模擬 20 人的成績
y = sample(1:100,20)
max0 = max(max(x),max(y))		# 取出所有成績的最大值
plot(x,xlim=c(0,28),type="b")
lines(1:20,y,lty=2, type="b",col=2)	

# 說明方塊的左上角參考點在 (22, 成績最大值)

legend(22,max0,legend=c("A 教學法","B 教學法"), col=c(1,2),lty=c(1,2))


