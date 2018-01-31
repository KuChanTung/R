x=read.table("/Users/kuchantung/Documents/GitHub/R/Exercise/Data/txt/babies.txt",header = TRUE)
weight.diff=function(weight,standard)
{
  difference=weight-standard
  return(difference)
}
bwt2=x$bwt
summary(bwt2)
Q1=quantile(bwt2,0.25)
n=length(bwt2)
weight2=character(n)
for (i in 1:n) {
  if(weight.diff(bwt2[i],Q1)<=0){
    weight2[i]="Low"
  }else{
    weight2[i]="OK"
  }
}
x$weight2 = weight2
write.table(x,"/Users/kuchantung/Documents/GitHub/R/Exercise/Data/txt/babies.txt",quote = FALSE,row.names = FALSE)