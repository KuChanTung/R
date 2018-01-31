# Example 7-4: switch application

center = function(x, type) 
{
      switch(type, mean = mean(x),
        		median = median(x),
         		trimmed = mean(x, trim = .1))
}

x = rcauchy(10)	# 隨機產生 10 個 Caucy 分配亂數

center(x, "mean")
center(x, "median")
center(x, "trimmed")

