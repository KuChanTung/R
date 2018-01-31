#############################################################################
# detectTC, detectLS : 丁计 TC 籔 LS 篈瞒竤盎代
#
# 睭厩参璸╰ 朝春不 2010/2/20
# 
# 膀セ粂猭:    detectTC(arimaン)
#              detectLS(arimaン)
#
# ㄤい arima ン琌竒筁 arima ㄧ计矪瞶筁块跑计
#      ㄤ緇把计ノ猭叫把σ TSA 甅ン detectAO ㄧ计弧
#
# 爹detectTC 籔 detectLS э TSA 甅ン detectAO ㄧ计
#############################################################################

detectTC = function (object, alpha = 0.05, delta = 0.7, cutoff = 0, robust = TRUE) 
{
    resid = residuals(object)
    
    piwt = ARMAtoMA(ar = -object$mod$theta, ma = -object$mod$phi, 
              lag.max = length(resid) - 1)

    n.piwt = length(piwt)
    
    x = numeric(n.piwt)
    
    for (k in 1:n.piwt)
    {
      if (k == 1)
         x[k] = delta - piwt[1]
      else
      {
         sum = 0
         for (j in 1:(k-1)) sum = sum + delta^(k-j)*piwt[j]
         x[k] = delta^k - sum - piwt[k]           
      }
    }
    
    x = c(1,-1*x)
    
    omega = filter(c(0 * resid[-1], rev(resid)), filter = x, side = 1, method = "convolution")
    
        
    omega = omega[!is.na(omega)]
    
    rho2 = 1/cumsum(x^2)      
    omega = omega * rho2
    
    if (robust) 
        sigma = sqrt(pi/2) * mean(abs(resid), na.rm = TRUE)        
    else sigma = object$sigma2^0.5
    
    lambda2T = omega/sigma/sqrt(rho2)
    lambda2T = rev(lambda2T)
    
    if (cutoff < 0.5)
       cutoff = qnorm(1 - alpha/2/length(lambda2T))      
   
    out = abs(lambda2T) > cutoff
    ind = seq(lambda2T)[out]
    lambda2 = lambda2T[out]
    
    if (length(ind) != 0) 
        print(rbind(ind, lambda2))
    else print("No TC-outlier detected")
    
    invisible(list(lambda2 = lambda2, ind = ind))
}

detectLS = function (object, alpha = 0.05, cutoff = 0, robust = TRUE) 
{
    resid = residuals(object)
    
    piwt = ARMAtoMA(ar = -object$mod$theta, ma = -object$mod$phi, 
              lag.max = length(resid) - 1)

    n.pi = length(piwt)
    
    x = numeric(n.pi)
    
    for (k in 1:n.pi)
    {
         sumw = sum(piwt[1:k])
         x[k] = 1 - sumw           
    }
    
    x = c(1,-1*x)
    
    omega = filter(c(0 * resid[-1], rev(resid)), filter = x, side = 1, method = "convolution")
           
    omega = omega[!is.na(omega)]
    
    rho2 = 1/cumsum(x^2)    
    
    omega = omega * rho2
    
    if (robust) 
        sigma = sqrt(pi/2) * mean(abs(resid), na.rm = TRUE)        
    else sigma = object$sigma2^0.5
    
    lambda2T = omega/sigma/sqrt(rho2)
    lambda2T = rev(lambda2T)

    if (cutoff < 0.5 )    
       cutoff = qnorm(1 - alpha/2/length(lambda2T))
    
    out = abs(lambda2T) > cutoff
    ind = seq(lambda2T)[out]
    lambda2 = lambda2T[out]
    
    if (length(ind) != 0) 
        print(rbind(ind, lambda2))
    else print("No LS-outlier detected")
    
    invisible(list(lambda2 = lambda2, ind = ind))
}


