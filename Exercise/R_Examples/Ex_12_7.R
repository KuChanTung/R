# Example 12-7: Convergence in Probability

library(ConvergenceConcepts)

samplegen <- function(n) {
     Z <- runif(1)
     k <- floor(log2(1:n))
     m <- 1:n - 2^k
     res <- (m * 2^(-k) <= Z & Z < (m + 1) * 2^(-k))
     return(as.integer(res))
}

critp = criterion(data, epsilon = epsilon0, mode = "p")$crit
critas = criterion(data, epsilon = epsilon0, mode = "as")$crit

plot(critp[1:1000],type="l")
lines(1:1000,critas[1:1000],col=2)

