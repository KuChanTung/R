# Example 13-5

# scores.U 與scores.L 儲存成績的分組上界跟下界，freq 是各組人數：
scores.L = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)

scores.U = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

freq = c(4, 7, 9, 6, 8, 9, 9, 2, 4, 2)

scores.mid = (scores.U + scores.L)/2
scores.mid

n = sum(freq)		# n = 樣本數
m = length(freq)		# m = 組數
f.sum = sum(freq*scores.mid)
f.mean = f.sum/n
f.mean

f.sum2 = sum(freq*(scores.mid - f.mean)^2)
f.s2 = f.sum2/n 
f.s2

f.s = sqrt(f.s2)		# 分組資料的樣本標準差
f.s

f.mode = scores.mid[which(freq == max(freq) ) ] 
f.mode

scores.U[m] - scores.L[1]	# m 為組數

quantile.freq = function(k, U, L, f)
{
	# k 介於0~100, U、L 為儲存各組上下界限值的向量，f 為次數向量

	n = sum(f)				# n = 觀察值總數
	p = ceiling((k/100)*n)
	csum = cumsum(f)
	j = sum(csum < p) + 1			# 找出百分位數所在的組別 j
	Cj1 = csum [j-1]/n				# 至第j-1組的累積相對次數比例
	Wj = U[j] - L[j]				# 第 j 組的組距
	Rj = f[j]/n				# 第 j 組的相對次數比例
	p.k = L[j-1]+ Wj*(k/100 - Cj1)/Rj
	return(p.k)
}

quantile.freq(50,U=scores.U,L=scores.L,f=freq)

quantile.freq(25,U=scores.U,L=scores.L,f=freq)		# Q1

quantile.freq(75,U=scores.U,L=scores.L,f=freq)		# Q3

