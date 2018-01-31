#############################################################################
# polr.compare : 比較兩個不同個體在 Ordinal Logistic 模型的機率對比
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 語法:       polr.compare(X1,X2,Y1,Y2, data=資料框架名稱, model=polr物件)
#
# 其中   X1, X2 為儲存兩個不同個體解釋變數值的 list 變數，
#        Y1 與 Y2 為想要計算機率值的分類名稱，
#        model 是經過 polr 函數分析過的輸出物件
#
# 如果模型的解釋變數是分類變數（儲存成 factor 或 ordered fator），
# 則 X1, X2 中寫到這個解釋變數的值時，必需當作字串，兩端加上雙引號。
# Y1 與 Y2 的值也需加上雙引號
#
# Example:
#
# > library(MASS)
# > data(Arthritis,package="vcd")
# > result.polr = polr(Improved ~ Treatment + Sex + Age,
#                         data=Arthritis, Hess=T, method="logistic")
#
# > x1 = list(Treatment="Placebo",Sex="Male",Age=35)
# > x2 = list(Treatment="Treated",Sex="Female",Age=48)
# > 
# > polr.compare(X1=x1,X2=x2,Y1="None",Y2="Some", 
# +                  data=Arthritis, model=result.polr)
#
#        Treatment    Sex Age Improved      prob
# Object1   Placebo   Male  35     None 0.9204108
# Object2   Treated Female  48     Some 0.2033733
#
# Pr(Y1 = None ) vs. Pr(Y2 =  Some ) odds :  4.53 
#
#############################################################################

polr.compare = function(X1,Y1,X2,Y2,data,model)
{
   require(MASS)
   
   if (missing(X1) || missing(X2) || missing(Y1) || missing(Y2) ||
       missing(data) || missing(model) )
   {
      return(cat("Error: some argument is not provied! \n"))
   }
   n.x = length(X1)
   Yname = names(attr(result.polr$terms,"dataClasses"))[1]
   
   X1.p = predict(model,newdata=X1,type="probs")
   X2.p = predict(model,newdata=X2,type="probs")
   Y.category = rbind(Y1,Y2)
   p1 = as.numeric(X1.p[which(names(X1.p) == Y1)])
   p2 = as.numeric(X2.p[which(names(X2.p) == Y2)])
   p12 = rbind(p1,p2)
   result = data.frame(rbind(Object1=X1,Object2=X2),Y.category, prob=p12)   
   names(result)[n.x+1] = Yname   
   print(result)

   cat("\nPr(Y1 =",Y1,") vs. Pr(Y2 = ",Y2,") odds : ",round(p1/p2,2),"\n")
}         