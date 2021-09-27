############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Truncates small and negatives eigenvalues to make estimated covariance matrix
#          positive-semi definite
# Updated: Aug 4, 2018

trunc.mat <- function(b.fit, R0, Ra, nb, fast.tn = F) {
#ptm <- proc.time()

  if (fast.tn) {
    alpha <- b.fit$B.est %*% (R0 - Ra)
    Delta <- matrix(b.fit$G %*% alpha, nrow = nb)
  } else {
    Theta0 <- trunc.mat.theta(b.fit, R0, nb, fast.tn = F)
    Thetaa <- trunc.mat.theta(b.fit, Ra, nb, fast.tn = F)
    Delta <- Theta0 - Thetaa
  }

  return(Delta)
#print(proc.time() - ptm)
}

trunc.mat.theta <- function(b.fit, R, nb, fast.tn = F) {
  alpha <- b.fit$B.est %*% R
  Theta <- matrix(b.fit$G %*% alpha, nrow = nb)
  if (!fast.tn) {
    Theta <- trunc.mat.inner(Theta)
  }
  return(Theta)
}

trunc.mat.inner <- function(Theta) {
  eigen.fit <- eigen(Theta, symmetric = T)
  efuncs <- eigen.fit$vectors
  evals <- as.numeric(eigen.fit$values)
  evals[evals < 1e-5] <- 0

  return(matrix.multiply(efuncs, evals) %*% t(efuncs))
}
