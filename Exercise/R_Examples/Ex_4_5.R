# Example 4-5: matrix multiplication in Regression Coefficient Estimation

Y = iris$Petal.Width
X = cbind(1,iris$Sepal.Length)	# X 是設計矩陣
X
XX = t(X) %*% X			# XX 相當於 XtX
XX

beta.hat = solve(XX) %*% t(X) %*% Y
beta.hat
x = iris$Sepal.Length
lm(Y ~ x)
