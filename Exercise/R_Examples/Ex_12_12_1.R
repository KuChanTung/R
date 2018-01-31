# Example 12-12: N(0,1) 查表表格

s1 = seq(0,3,0.1)
s2 = seq(0,0.09,0.01)
x = matrix(0,length(s1),length(s2))

for(i in 1:length(s1))
{
  for (j in 1:length(s2))
  {
    q1= s1[i]
    q2= s2[j]
    q = q1+q2
    x[i,j] = round(pnorm(q),4)
  }
}

colnames(x) = paste("0.0",0:9,sep="")
rownames(x) = format(seq(0.0,3.0,0.1))  # format 讓 2 顯示成 2.0

# 或使用以下程式
# s1 = seq(0,3,0.1)
# s2 = seq(0,0.09,0.01)
# myfun = function(q1,q2) round(pnorm(q1+q2),4)
# m = outer(s1,s2,myfun)
# colnames(m) = paste("0.0",0:9,sep="")
# rownames(m) = format(seq(0.0,3.0,0.1))  
# m
#
