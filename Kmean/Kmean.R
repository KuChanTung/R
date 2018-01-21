########################################
##美國2011年至2014年城鎮房地產銷售紀錄##
########################################
#讀入資料集
library(data.table)
HousePrice<-fread("RealEstateSalesTown2011TO2014OK.csv",sep = ",",stringsAsFactors = F,fill = T,verbose = T)
#列出資料維度
dim(HousePrice)
#將資料集城市名轉換為字元
var=HousePrice$Name
var=as.character(var)
#將資料集房屋預估價與實際銷售價轉換為數字
HA=HousePrice$AssessedValue
HA=as.numeric(HA)
HS=HousePrice$SalePrice
HS=as.numeric(HS)
head(var)
head(HousePrice)
#畫出所有城市預估價格與實際成交價的樣本點
plot(HA,HS)
#找出預估價最高的城市
MA=which.max(HA)
#找出成交價最高的城市
MS=which.max(HS)
#標出預估價與成交價最高的樣本點
points(HousePrice[c(MA,MS),-1],pch=16)
legend(HA[MA],HS[MA],var[MA],bty = "n",xjust = 1,cex = 8)
#利用房產成交價格畫出點圖
library(epicalc)
dotplot(HS,pch = 16,axes=F)
#利用房屋預估價畫出點圖
dotplot(HA,pch = 16,axes=F)
#取出房產銷售的敘述統計數據
my.desc=function(x)
{
  n=length(x)
  x.desc <- c(n,summary(x),var(x),sum(x),sqrt(var(x)),IQR(x),mad(x))
  names(x.desc) <- c("樣本數","最小值","Q1","中位數","平均數","Q3","最大值","變異數","總和","標準差","IQR","MAD")
  return(x.desc)
}
my.desc(HS)
my.desc(HA)

#利用K平均值分群
fit_kml=kmeans(HousePrice[,-1],centers = 3)
print(fit_kml)
#取得各種別中心座標
fit_kml$centers
#分群總平方和
fit_kml$totss
#組內平方和
fit_kml$tot.withinss
#組間平方和
fit_kml$betweenss
#組間平方和＋組內平方和=總平方和
fit_kml$betweenss+fit_kml$tot.withinss
#圖形表示(用三種不同標示)
plot(HousePrice[,-1],pch=(fit_kml$cluster-1))
points(fit_kml$centers,pch=8)
legend(fit_kml$centers[1,1],fit_kml$centers[1,2],"Center_1",bty="n",xjust = 1,yjust = 0,cex = 0.8)
legend(fit_kml$centers[2,1]-2,fit_kml$centers[2,2],"Center_2",bty="n",xjust =0,yjust = 0,cex = 0.8)
legend(fit_kml$centers[3,1],fit_kml$centers[3,2],"Center_3",bty="n",xjust =0.5,cex = 0.8)
#設定result變數儲存分群優度值
result=rep(156543)
for(k in 1:156543)
{
  fit_km=kmeans(HousePrice[,-2],center = K )
  result[K]=fit_km$betweenss/fit_km$totes
}
round(result,2)