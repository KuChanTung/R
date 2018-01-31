# Example 1-3

#  請修改資料檔路徑
x = read.table("c:/r/babies.txt",header=TRUE)   

# 簡單的自訂函數 weight.diff ：算出兩個輸入數目 weight 
# 與 standard 的差異值，以判斷初生嬰兒體重是否在前 25% 以下

weight.diff = function(weight, standard)
{
  difference = weight – standard
  return( difference )
}

# 令新變數 bwt2 = x 變數裡面 bwt 那一直行(column) 所有的資料
bwt2 = x$bwt

# 算出初生嬰兒體重的敘述統計量
summary(bwt2)

# 算出 Q1 = first quartile = 25% percentile
Q1 = quantile(bwt2, 0.25)

# 計算 bwt2 向量中所有元素的個數
n = length(bwt2)

# 先宣告一個空的文字向量變數 weight2 ，預留 n 個元素的位置
# 若要宣告成數值向量，則用 weight2 = numeric(n)
weight2 = character(n)

# 使用迴圈一一計算每個初生嬰兒體重是否過輕
for (i in 1:n)
{
# 使用自訂函數 weight.diff 判斷嬰兒體重是否小於 Q1
  if ( weight.diff(bwt2[i], Q1) <= 0 ) {     
		weight2[i] = "Low"
  } else {
		weight2[i] = "OK"
  }
}
# 將新產生的 weight2 向量變數加入原本的 data-frame 變數 x 中
x$weight2 = weight2

# 將新的資料寫到外部檔案。輸出時，變數名稱不加引號，
# 所有橫列最左邊不印出 1, 2, 3,...等序號
write.table(x,"c:/R/new_babies.txt",quote=FALSE, 	row.names=FALSE)  


