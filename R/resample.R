############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Generate bootstrap resamples
# Updated: Aug 4, 2018

resample <- function(data, mu, coef.null, sigsq, L = 1) {
  nsubj <- length(unique(data$.id))
  cov.mat <- matrix(c(coef.null[1], coef.null[2], coef.null[2], coef.null[3]), nrow = 2)
  b.mat <- matrix(rnorm(2 * nsubj * L), ncol = 2) %*% chol(cov.mat) # random effects

  # r.slope & r.int by subject
  par.random <- sapply(seq_len(L), function(i) {
        s_offset <- (i-1) * nsubj
        sapply(seq_len(nsubj), function(x) {
            b.mat[s_offset + x, 1] + b.mat[s_offset + x, 2] * subset(data, .id == unique(data$.id)[x])$.index
        })
    })
  if (is.list(par.random)) { # if a list
    par.random <- unlist(par.random)
  }
  # mean.null + null.par r.int & r.slope*t + alt.par residual
  matrix(mu + par.random + rnorm(nrow(data) * L, sd = sqrt(sigsq)), ncol = L)
}