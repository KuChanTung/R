# Example 7-6 : Taylor Expansion for f(x) = 1/x

fx.real = 1/0.3
fx.real
fx.taylor = 1.0
i = 1
acc = 1.0
while(acc > 0.00001)
{
   fx.taylor = fx.taylor + (-1)^(i %% 2)*(0.3 - 1)^i
   acc = abs(fx.taylor - fx.real)
   i = i+1
}
 
fx.taylor


