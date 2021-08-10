############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Generate bootstrap resamples
# Updated: Aug 4, 2018

resample <- function(data, mu, coef.null, sigsq) {
  nsubj <- length(unique(data$.id))
  cov.mat <- matrix(c(coef.null[1], coef.null[2], coef.null[2], coef.null[3]), nrow = 2)
  b.mat <- matrix(rnorm(2 * nsubj), ncol = 2) %*% chol(cov.mat) # random effects

  # r.slope & r.int by subject
  par.random <- sapply(c(1:nsubj), function(x) b.mat[x, 1] + b.mat[x, 2] * subset(data, .id == x)$.index)
  if (is.list(par.random)) { # if a list
    par.random <- unlist(par.random)
  }
  # mean.null + null.par r.int & r.slope*t + alt.par residual
  data.bs <- data.frame(
    .value = mu + c(par.random) + rnorm(nrow(data), sd = sqrt(sigsq)),
    .index = data$.index, .id = data$.id
  )
  return(data.bs)
}