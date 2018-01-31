# Example 1-1

# 台北市、高雄市、基隆市、新竹市、台中市 1999~2008 流浪狗被收養數
# 將資料儲存在一個向量變數：dogs.adopted

dogs.adopted = c(34451,11403,3907,5027, 9006)
mean(dogs.adopted)		# 求出樣本平均數
sd(dogs.adopted)

# 引用 Ryacas 套件(package)
library(Ryacas)
x = Sym("x")
# 求出 sin(ax)2cos(bx) 的符號積分
PrettyForm(yacas("Integrate(x) Sin(a*x)^2*Cos(b*x)"))

# 畫出 3D 函數透視圖
x = y = seq(-15,15,0.5)	
fxy = function(x,y) sin(sqrt(x^2+y^2))/sqrt(x^2+y^2)
z = outer(x,y,fxy)     	
persp(x,y,z, phi=20, theta=30 )

# 產生 100 組 N(0,1) 樣本(樣本數 500)，並畫出 100 個併排的直方圖
oldpar=par()
devAskNewPage(ask = TRUE)
par(mfrow=c(10,10),mai=c(0.1,0.1,0.1,0.1))   
for (i in 1:100) hist(rnorm(500,0,1),main="")
par(oldpar)