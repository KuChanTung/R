# Ex 14-3 : sigma test

# 請修改資料檔路徑
babies = read.table("c:/r/babies.txt",header=T)
bwt = babies$bwt

n = length(bwt)					# 樣本數 n
sigma.square = 332.5				# σ2
S.square = var(bwt)					# S2
test.stat = (n-1)*S.square / sigma.square		# 卡方檢定值
c1 = qchisq(0.025,n-1)				# 卡方分配左尾臨界值
c2 = qchisq(0.975,n-1)				# 卡方分配右尾臨界值
cat("卡方檢定值 = ",test.stat,"\n")
cat("左、右尾臨界值：",c1,",",c2,"\n")
if( test.stat < c1 || test.stat > c2 ) {
	cat("拒絕 Ho! \n") 
} else {
	cat("不拒絕 Ho \n")
}
# 計算 P-value
if (S.square < sigma.square ){
	p.value = 2*pchisq(test.stat,n-1)
} else {
	p.value = 2*(1 - pchisq(test.stat,n-1))
}

cat("P-value = ",p.value,"\n")

