#############################################################################
# logistic.compare : 比較兩個不同個體在 二分類 Logistic 模型的機率對比
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 語法:       logistic.compare(X1,X2,Y1,Y2, data=資料框架名稱, model=polr物件)
#
# 其中   X1, X2 為儲存兩個不同個體解釋變數值的 list 變數，
#        Y1 與 Y2 為想要計算機率值的分類名稱，
#        model 是經過 glm 函數分析過的輸出物件
#
# 如果模型的解釋變數是分類變數（儲存成 factor 或 ordered fator），
# 則 X1, X2 中寫到這個解釋變數的值時，必需當作字串，兩端加上雙引號。
# Y1 與 Y2 的值也需加上雙引號
#
# 例如，假設 students 這個資料框架變數裡面包含應變數 admission (是否申請入學成功，
# 兩個分類為 "No" 與 "Yes")，以及解釋變數 age 與 gender (gender 儲存為 factor 變數)。
#
# 假設第一個學生 19 歲, 男性，第二學生 21 歲, 女性。若我們想比較
# 第一個學生申請成功(admission="Yes") vs. 第二個學生申請失敗(admission="No") 
# 的機率勝算(odds)：
#
# > source("c:/r/logistic_compare.R")
# > mymodel = glm( admission ~ age + gender, data=students,
# 				family = binomial(link = logit) )
# > x1 = list(age=19, gender="Male")
# > x2 = list(age=21, gender="Female")
# > logistic.compare(X1=x1, Y1="Yes", X2=x2, ,Y2="No", 
#         data=students, model=mymodel)
#
#############################################################################

logistic.compare = function(X1,Y1,X2,Y2,data,model)
{
   require(MASS)
   
   if (missing(X1) || missing(X2) || missing(Y1) || missing(Y2) ||
       missing(data) || missing(model) )
   {
      return(cat("Error: some argument is not provied! \n"))
   }
   n.x = length(X1)
   Yname = names(attr(model$terms,"dataClasses"))[1]
   Y0 = levels(babies[,Yname])[1]
   
   
   X1.p = predict(model,newdata=as.data.frame(X1),type="response")
   X2.p = predict(model,newdata=as.data.frame(X2),type="response")
   Y.category = rbind(Y1,Y2)
   p1 = X1.p
   p2 = X2.p
   if (Y1 == Y0)
     p1 = 1-p1
   if (Y2 == Y0)
     p2 = 1-p2     
   p12 = rbind(p1,p2)
   result = data.frame(rbind(Object1=X1,Object2=X2),Y.category, prob=p12)   
   names(result)[n.x+1] = Yname   
   print(result)

   cat("\nPr(Y1 =",Y1,") vs. Pr(Y2 = ",Y2,") odds : ",round(p1/p2,2),"\n")
}         
