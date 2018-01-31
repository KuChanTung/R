#############################################################################
# predictTSA : TSA 套件 arima 函數的預測函數
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 本程式內部主要運算部分，參考自 TSA 套件的 arima 函數內容
# 
# 語法:  predictTSA(model, n.ahead=k, newxreg=Xreg, newxtransf=Xtrans, transfer )
#				
# 其中 
#	model 為經過 TSA 套件 arima 函數分析過的輸出物件，
#
#         newxreg 是新的解釋變數值所組成的矩陣，通常的結構是 cbind(X1new, X2new,....)
#
#         newxtransf 是 TSA 套件 arima 函數 xtrans 變數新舊觀察值所組成的矩陣，
#                通常的結構是 cbind(trans變數1, trans變數2,...)
#
#                每個 trans 變數內容都包含舊的觀察值與新的觀察值，以便利用落後期數的資訊
#                例如， new.trans.X1 = c(old.transX1, new.transX1.values)
#
#  Example: 請參考本書程式範例 18-3
#
# > model = arima(CO2,c(2,0,0),,include.mean=T, xtransf=cbind(gasRate3), transfer=list(c(1,2)))
#
# > CO2.predict = predictTSA(model, n.ahead=10, newxtransf=cbind(newGasRate),transfer=list(c(1,2)))
#
#############################################################################

predictTSA = function(model, n.ahead=1,newxreg=NULL,newxtransf=NULL,transfer=NULL )
{
    require(TSA)
    
    armafilter = function(x, phi, theta, Delta) 
    {
        if (length(theta) > 0) 
            x = filter(x, filter = theta, side = 1, method = "convolution")
            
        if (length(phi) > 0 && any(phi != 0)) 
            x = filter(x, filter = phi, side = 1, method = "recursive")
            
        if (length(Delta) > 0) 
            x = filter(x, filter = Delta, side = 1, method = "recursive")
        x
    }
    
    addtransfer = function(pred, n.head, coef, ncoef0, newxtransf, transfer)
    {
       ind = 0
       for (pq in transfer) 
       {
          ind = ind + 1
          p = pq[1]
          q = pq[2]

          if (p > 0) 
              phi = coef[ncoef0 + (1:p)]
          else phi = NULL

          if (q > 0) 
              theta = coef[ncoef0 + p + (1:q)]
          else theta = NULL
            
          xx = armafilter(newxtransf[, ind], phi = phi, theta = theta, Delta = NULL)
          idx2 = length(newxtransf[,ind])
          idx1 = idx2 - n.ahead + 1
          xx = xx[idx1:idx2]
          pred = pred + xx
       }
       return(pred)
    }
             
    if (!missing(transfer)) 
    {
        for (i in seq(length(transfer))) 
           transfer[[i]][2] = transfer[[i]][2] +  1
    }
    
    if (!missing(newxreg))
    {
      nxreg = ncol(newxreg)
    } else  nxreg = 0
    
  
    model2 = model 
    coef.all = model$coef
    
    ncoef0 = model$arma[1] + model$arma[2] + model$arma[3]+ model$arma[4] + nxreg +1
    model2$coef = coef(model)[1:ncoef0]
    
    xpred = predict(model2,newxreg=newxreg,n.ahead=n.ahead)    
    
    pred0 = xpred$pred
    se0 = xpred$se
    
    pred = addtransfer(pred0, n.ahead, coef.all, ncoef0, newxtransf, transfer)

    return(list(pred=pred,se=se0))
}

