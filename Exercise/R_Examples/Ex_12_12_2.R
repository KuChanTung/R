# Example 12-12: t 分配查表表格

df = 1:20
p = c(0.10,0.05,0.025,0.01)
myfunc = function(df,p) round(qt(p,df,lower.tail=F),4)
m = outer(df,p,myfunc)
colnames(m) = format(P)
rownames(m) = format(1:20)
m 


