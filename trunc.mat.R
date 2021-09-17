############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Truncates small and negatives eigenvalues to make estimated covariance matrix
#          positive-semi definite
# Updated: Aug 4, 2018

trunc.mat <- function(b.fit, R, nb, trunc = F) {
#ptm <- proc.time()
  Theta <- matrix(b.fit$B.est %*% R, nrow = nb)
  Theta <- 0.5 * (Theta + t(Theta))
  if (trunc) {
    eigen.fit <- eigen(Theta)
    efuncs <- eigen.fit$vectors
    evals <- as.numeric(eigen.fit$values)
    evals[evals < 1e-5] <- 0
    Theta <- efuncs %*% tcrossprod(diag(evals), efuncs)
  }
  as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta), b.fit$Bstar))
#print(proc.time() - ptm)
}
