# Example 1-2

# 從 N(5,10) 機率分配產生 100 個隨機亂數，存到 y 向量
y = rnorm(100,5,10)  

# Q1 = 第 25 百分位數，Q3 = 第 75 百分位數
Q1 = quantile(y, 0.25)      
Q3 = quantile(y, 0.75)      

stat.names = c("最小值","Q1","平均數","中位數","Q3","最大值")
s = c(min(y), Q1, mean(y), median(y), Q3, max(y))
names(s) = stat.names
print(s)

# 畫出 y 的直方圖，縱軸採用機率值而非次數
hist(y,prob=TRUE)            

# 畫出資料的經驗機率密度曲線(empirical density)，使用虛線
lines(density(y),lty=2)
# 畫出 Normal(5,10) 的理論機率密度曲線
curve(dnorm(x,5,10),min(y),max(y),n=100,add=TRUE) 

