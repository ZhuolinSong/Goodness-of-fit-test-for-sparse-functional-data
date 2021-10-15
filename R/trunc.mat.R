############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Truncates small and negatives eigenvalues to make estimated covariance matrix
#          positive-semi definite
# Updated: Aug 4, 2018

trunc.mat <- function(b.fit, R0, Ra, nb, truncate.tn = 1) {
#ptm <- proc.time()

  if (truncate.tn == 0) {
    alpha <- b.fit$B.est %*% (R0 - Ra)
    Delta <- matrix(b.fit$G %*% alpha, nrow = nb)
  } else {
    Theta0 <- trunc.mat.theta(b.fit, R0, nb, truncate.tn)
    Thetaa <- trunc.mat.theta(b.fit, Ra, nb, truncate.tn)
    Delta <- Theta0 - Thetaa
  }

  return(Delta)
#print(proc.time() - ptm)
}

trunc.mat.theta <- function(b.fit, R, nb, truncate.tn = 1) {
  alpha <- b.fit$B.est %*% R
  Theta <- matrix(b.fit$G %*% alpha, nrow = nb)
  if (truncate.tn != 0) {
    Theta <- trunc.mat.inner(b.fit, Theta, truncate.tn)
  }
  return(Theta)
}

trunc.mat.inner <- function(b.fit, Theta, truncate.tn = 1) {
  if (truncate.tn == 1) {
    eigen.fit <- eigen(Theta, symmetric = T)
    efuncs <- eigen.fit$vectors
  } else if (truncate.tn == 2) {
    Theta <- as.matrix(b.fit$Xstar.half %*% Matrix(Theta) %*% b.fit$Xstar.half)
    eigen.fit <- eigen(Theta, symmetric = T)
    efuncs <- b.fit$Xstar.invhalf %*% eigen.fit$vectors
  }
  evals <- as.numeric(eigen.fit$values)
  evals[evals < 1e-5] <- 0
  return(matrix.multiply(efuncs, evals) %*% t(efuncs))
}
