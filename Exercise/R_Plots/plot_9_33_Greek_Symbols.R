#############################################################################
# 畫出 圖 9-33 : plotmath 希臘字母列表對照圖
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
#
# 請將圖形視窗調成較大的 size
#############################################################################
greeks = c(
"alpha",   "beta",    "gamma" ,  "delta"  , "epsilon", "zeta"   , "eta" ,   
"theta",   "iota",    "kappa" ,  "lambda" , "mu"     , "nu"     , "xi"  ,   
"omicron", "pi"  ,    "rho"   ,  "sigma"  , "tau"    , "upsilon", "phi" ,   
"chi",     "psi" ,    "omega" ,  "Alpha"  , "Beta"   , "Gamma"  , "Delta",  
"Epsilon", "Zeta" ,   "Eta"   ,  "Theta"  , "Iota"   , "Kappa"  , "Lambda", 
"Mu",      "Nu"   ,   "Xi"    ,  "Omicron", "Pi"     , "Rho"    , "Sigma" , 
"Tau",     "Upsilon", "Phi"   ,  "Chi"    , "Psi"    , "Omega"  
)

plot(0:7,xlim=c(0,7),ylim=c(0.7,8),xlab="",ylab="",type="n",axes = F)
k = 0
nr = 8
nc = 6
abline(v=0.15,col="grey")
for(i in 1:nc)
{
   for (j in 1:nr)
   {
      k = k + 1
      greek = greeks[k]
      g0 = as.name(greek)
      text(i,nr-j+1,g0)
      text(i-0.5,nr-j+1,greek)
   }
   abline(v=i+0.15,col="grey")
}


