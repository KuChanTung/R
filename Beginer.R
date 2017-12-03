with(iris,{
  mean(Sepal.Length)
  mean(Petal.Length)
  lm(Sepal.Length~Petal.Length)
})

my.sd <- function(y)
{
  n=length(y)
  s=sqrt((sum(y^2)-n*mean(y)^2)/(n-1))
  return(s)
}
