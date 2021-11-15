############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Truncates small and negatives eigenvalues to make estimated covariance matrix
#          positive-semi definite
# Updated: Aug 4, 2018

#' @import matrixcalc
#' @import Matrix

trunc.mat <- function(b.fit, R0, Ra, nb, truncate.tn = 1, ic = 0) {
#ptm <- proc.time()

  if (truncate.tn == 0) {
    alpha <- b.fit$B.est %*% (R0 - Ra)
    Delta <- matrix(b.fit$G %*% alpha, nrow = nb)
  } else {
    Theta0 <- trunc.mat.theta(b.fit, R0, nb, truncate.tn, ic)
    Thetaa <- trunc.mat.theta(b.fit, Ra, nb, truncate.tn, ic)
    Delta <- Theta0 - Thetaa
  }

  return(Delta)
#print(proc.time() - ptm)
}

trunc.mat.theta <- function(b.fit, R, nb, truncate.tn = 1, ic = 0) {
  alpha <- b.fit$B.est %*% R
  if (ic == 1) {
    Theta <- fixed_PG(b.fit, R, nb, alpha, 1e3, 1e-3)
  } else if (ic == 2) {
    Theta <- A_PG(b.fit, R, nb, alpha, 1e3, 1e-3)
  } else if (truncate.tn != 0) {
    Theta <- matrix(b.fit$G %*% alpha, nrow = nb)
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

trunc.mat.approx <- function(b.fit, R, nb, truncate.tn = 1) {
  alpha <- b.fit$B.est %*% R
  Theta <- matrix(b.fit$G %*% alpha, nrow = nb)
  Gmat <- as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta), b.fit$Bstar))
  Gmat2 <- 0.5 * (Gmat + t(Gmat))
  # truncation
  if (truncate.tn) {
    eigen.fit <- eigen(Gmat2)
    efuncs <- eigen.fit$vectors
    evals <- as.numeric(eigen.fit$values)
    evals[evals < 1e-5] <- 0
    Gmat2 <- efuncs %*% tcrossprod(diag(evals), efuncs)
  }
  return(Gmat2)
}


fixed_PG <- function(b.fit, R, nb
                    , alpha = rep(1/sqrt(nb*(1 + nb)/2), nb*(1 + nb)/2)
                    , max_iter = 1e3, tol = 1e-3, truncate.tn = 2) {
  BtR <- crossprod(b.fit$B, R)
  grad <- b.fit$BtB %*% alpha - BtR
  temp <- sqrt(sum(grad^2))
  # choose step size
  t <- 1/norm(b.fit$BtB, "2")
  for (i in 1:max_iter) {
    # gradient update + convert to matrix
    alpha <- alpha - t * grad
    Theta <- matrix(b.fit$G %*% alpha, nrow = nb)

    # truncation
    Theta <- trunc.mat.inner(b.fit, Theta, truncate.tn)

    # convert to vector + gradient calculation
    alpha <- vech(Theta)
    grad <- b.fit$BtB %*% alpha - BtR

    # convergence
    temp <- sqrt(sum(grad^2))

    if (temp < tol) {
      break
    }
  }
  # print(temp)
  return(Theta)
}

A_PG <- function(b.fit, R, nb
                , alpha = rep(1/sqrt(nb*(1 + nb)/2), nb*(1 + nb)/2)
                , max_iter = 1e3, tol = 1e-3, truncate.tn = 2) {
  BtR <- crossprod(b.fit$B, R)
  alpha_0 <- alpha; alpha_1 <- alpha

  # choose step size
  t <- 1/norm(b.fit$BtB, "2")

  for (i in 1:max_iter) {
    # velocity and gradient calculation
    velocity <- alpha_1 + (i-2)/(i+1)*(alpha_1 - alpha_0)
    grad <- b.fit$BtB %*% velocity - BtR

    # gradient update + convert to matrix
    alpha <- velocity - t * grad
    Theta <- matrix(b.fit$G %*% alpha, nrow = nb)

    # truncation(proximal update)
    Theta <- trunc.mat.inner(b.fit, Theta, truncate.tn)
    alpha <- vech(Theta)

    # convergence
    temp <- sqrt(sum(grad^2))

    if (temp < tol) {
      break
    }

    # assign alphas
    alpha_0 <- alpha_1; alpha_1 <- alpha
  }
  # print(temp)
  return(Theta)
}