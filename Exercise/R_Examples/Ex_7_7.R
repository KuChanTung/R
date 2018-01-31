# Example 7-7: Taylor Expansion for f(x) = 1/(1-x)

fx.real = 1/(1 - 0.3)
fx.real

fx.taylor = 1.0
i = 1
repeat
{
   fx.taylor = x.taylor + 0.3^i
   if (abs(fx.taylor - fx.real) < 0.00001) break
   i = i+1
}

fx.taylor

