# Example 17-4: c and u chart

library(qcc)
x = c(8, 10, 10, 7, 12, 11, 13, 10, 10,  8, 11,  5,  6, 10, 11, 12,  6,  5, 12, 11 , 6, 11,  6, 13,  9)
 
x.c = qcc(data=x,type="c")
x.c$center
x.c$limits

x2 = c(24, 24, 22, 19, 27, 23, 31, 22, 25, 36, 23, 21, 12, 31,
         26, 27, 31, 17, 26, 29, 31, 24, 27,16, 24)
units = c(3, 5, 3, 4, 5, 3, 3, 4, 3, 3, 4, 5, 4, 4, 5, 
          3, 5, 3, 3, 3, 3, 4, 3, 4, 5)
          
x.u = qcc(data=x2,sizes=units,type="u")
x.u$center


