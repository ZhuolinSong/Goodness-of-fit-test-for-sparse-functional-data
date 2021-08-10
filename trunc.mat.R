############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Truncates small and negatives eigenvalues to make estimated covariance matrix
#          positive-semi definite
# Updated: Aug 4, 2018

trunc.mat <- function(b.fit, R, times) {
  Gmat <- matrix(b.fit$Bstar.tensor %*% (b.fit$BtB.inv %*% crossprod(b.fit$B, R)), nrow = length(times))
  Gmat2 <- 0.5 * (Gmat + t(Gmat))
  eigen.fit <- eigen(Gmat2)
  efuncs <- eigen.fit$vectors
  evals <- as.numeric(eigen.fit$values)
  evals[evals < 1e-5] <- 0
  return(efuncs %*% tcrossprod(diag(evals), efuncs))
}