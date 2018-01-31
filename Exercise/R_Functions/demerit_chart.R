#############################################################################
# demerit.chart : 畫出品管 demerit 管制圖
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 語法: demerit.chart(X, demerit.type, weights, n)
#
# 其中	X = 包含各類缺點的矩陣或 data.frame 變數，每一種缺點佔一個 column(直行)
#         demerit.type = 列出所有缺點名稱的文字向量，需與 X 的直行名稱相對應
#         weights = 儲存各類缺點比重的數值向量
#         n = 每個 subsample 的 units 數目
#
# Example:
#
# > X = rbind(c(4, 10, 18),
# +       c(7, 14, 24),
# +       c(5, 13, 23),
# +       c(4,  4, 18),
# +       c(5,  8, 15),
# +       c(1, 12, 18))
# > colnames(X) = c("Critical","Major","Minor")
# > X
#     Critical Major Minor
# [1,]        4    10    18
# [2,]        7    14    24
# [3,]        5    13    23
# [4,]        4     4    18
# [5,]        5     8    15
# [6,]        1    12    18
#
# > demerit.chart(X,c("Critical","Major","Minor"),c(10,5,1),5)
#
# CL =  22.7 
# LCL, UCL = ( 6.748198  , 38.6518 ) 
#
#############################################################################

demerit.chart = function(X,demerit.types,weights, n)
{
  # 計算樣本組數
  g = nrow(X)
  # 計算共有多少種不合格項目分類
  n.type = length(demerit.types)  
  # 使用矩陣乘法計算各樣本的 u 值
  u = (as.matrix(X[,demerit.types])%*% weights )/n
  # 計算每一個不合格分類的平均缺點數
  ubar.types = apply(X[,demerit.types],2,sum)/(g*n)
  
  # 使用矩陣乘法計算中心線 u.bar
  u.bar = weights %*% ubar.types
  # 使用矩陣乘法計算 u 的標準差估計值
  sigma.u = sqrt( ubar.types %*% (weights^2)/n ) 

  LCL = u.bar - 3*sigma.u
  UCL = u.bar + 3*sigma.u

  cat("CL = ",u.bar,"\n")
  cat("LCL, UCL = (",LCL," ,",UCL,") \n")

  # 預留 Y 軸上下高度
  ymin = u.bar - 4*sigma.u
  ymax = u.bar + 4*sigma.u  
  
  plot(u, xlab="sample",ylab="Demerit",type="b", 
        ylim=c(ymin,ymax), main="Demerit 管制圖")

  abline(h=u.bar)	    # 畫出中心線
  abline(h=UCL,lty=2)
  abline(h=LCL,lty=2)
 
}

