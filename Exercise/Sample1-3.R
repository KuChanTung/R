mycompare=function(a,b){
  if(a>b)
    c="及格"
  else
    c="不及格"
  return(c)
}

set.seed(123454321)
scores=rnorm(10,60,5)
for (i in 1:10) {
  x=round(scores[i],1)
  cat(i,"分數",x,"",mycompare(x,60),"\n")
  }