#############################################################################
# quantile.freq 計算分組資料(grouped data) 的百分位數
#
# 作者：淡江大學統計系 陳景祥 2010/2/20
# 
# 語法:   quantile.freq(k, U, L, f)
#
# 其中	k = 百分位數值， 0 <= k <= 100
#	U = 儲存各組上界(upper limit) 的數值向量
#         L = 儲存各組下界(lower limit) 的數值向量
#         f = 儲存各組次數 (frequency)的向量
#
# Example:
# > Lower = c(0,21,41,61,81)
# > Upper = c(20,40,60,80,100)
# > freq = c(13,20,25,18,5)
# > quantile.freq(25, U=Upper,L=Lower,f=freq)
# [1] 6.8875
#
#############################################################################
quantile.freq = function(k, U, L, f)
{
	# k 介於 0~100, U、L 為儲存各組上下界限值的向量，f 為次數向量

	n = sum(f)		# n = 觀察值總數
	p = ceiling((k/100)*n)
	csum = cumsum(f)
	j = sum(csum < p) + 1	# 找出百分位數所在的組別 j
	Cj1 = csum [j-1]/n		# 至第j-1組的累積相對次數比例
	Wj = U[j] - L[j]		# 第 j 組的組距
	Rj = f[j]/n		# 第 j 組的相對次數比例
	p.k = L[j-1]+ Wj*(k/100 - Cj1)/Rj
	return(p.k)
}


