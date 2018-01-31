#############################################################################
# sigma.test : 母體標準差 sigma 檢定
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 語法: sigma.test(x, sigma=1.0, side="two", alpha=0.05)
#
#       x 為數值向量
#       sigma = 虛無假設 Ho 裡面所主張的 sigma 值
#       side = "two" (H1: sigma != sigma0), "left"  ( H1: sigma < sigma0), 
#              或 "right" (H1: sigma > sigma0)
#       alpha = Type I error of test
#
# Example:
#
# > x = rnorm(100,0,1)
# > sigma.test(x, sigma=1.0, side="two")
#
# H1: sigma !=  1 
# 卡方檢定值 (test value) =  119.0788 
# p-value =  0.08270033 
# 查表值 =  73.36108  and  128.422 
# 
# Decision: Do not reject Ho 
# 
# 樣本變異數  1.202816   樣本標準差  1.096730 
# 
# 母體標準差 (sigma) 的  95 % 信賴區間 : 
# 0.9629362 1.274044 
# 
# 母體變異數 (sigma^2)的  95 % 信賴區間 : 
# 0.9272462 1.623188 
# 
#############################################################################
sigma.test = function(x, sigma=1.0, side="two", alpha=0.05)
{
   n = length(x); xbar <- mean(x)
   n1 = n - 1
   s2 = var(x)
   se = sqrt(s2)
   test.value = n1*s2/(sigma*sigma)
   conf0 = 1.0 - alpha
   alpha2 = alpha/2.0
   conf02 = 1.0 - alpha2
   conf01 = alpha2
   crit02 = qchisq(conf02,df=n1)
   crit01 = qchisq(conf01,df=n1)
   if (side == "left")
   {
       conf.left = 1 - alpha
       crit2 = qchisq(conf.left,df=n1)
   }
   else if (side == "right")
   {
       conf.right = alpha
       crit1 = qchisq(conf.right,df=n1)
   }
   else
   {
       crit1 = crit01
       crit2 = crit02
   }
   #
   pvalue = 1.0 - pchisq(test.value,df=n1)
   if (side == "two")
   {
       H1 = paste("\nH1: sigma != ",sigma,"\n\n")
       result = (test.value > crit2) || (test.value < crit1)
   }
   else if (side == "left")
   {
       H1 = paste("\nH1: sigma < ",sigma,"\n\n")
       result = test.value > crit2
   }
   else
   {
       H1 = paste("\nH1: sigma > ",sigma,"\n\n")
       result = test.value < crit1
   }

   cat(H1)
   cat("卡方檢定值 (test value) = ",test.value,"\n")
   cat("p-value = ",pvalue,"\n")

   if (side == "two")
     cat("查表值 = ",crit1," and ",crit2,"\n")
   else if (side == "left")
     cat("查表值 = ",crit2,"\n")
   else
     cat("查表值 = ",crit1,"\n")

   if (result)
       cat("\nDecision: Reject Ho \n\n")
   else
       cat("\nDecision: Do not reject Ho \n\n")
   cat("樣本變異數 ",s2,"  樣本標準差 ",se,"\n\n")
   cat("母體標準差 (sigma) 的 ",conf0*100,"% 信賴區間 : \n")
   cat(sqrt(n1*s2/crit02),sqrt(n1*s2/crit01),"\n\n")
   cat("母體變異數 (sigma^2)的 ",conf0*100,"% 信賴區間 : \n")
   cat(n1*s2/crit02,n1*s2/crit01,"\n")
}

