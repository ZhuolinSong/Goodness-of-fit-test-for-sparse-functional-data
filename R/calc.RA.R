############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Calculates the Kronecker product of Y_i x Y_i for estimating alternative cov
# Note: Could be sped up if m_i equal for all subjects
# Updated: Aug 4, 2018

calc.RA <- function(data, include.diag = F) {
  y <- data$.value
  subj <- data$.id
  usubj <- unique(subj)
  n <- length(usubj)
  R <- c()

  for (i in 1:n) {
    index <- which(subj == usubj[i])
    mi <- length(index)
    yi <- y[index]

    if (mi > 1) {
      # <------ select off diagonal only
      sel <- setdiff(1:(mi * (mi + 1) / 2), c(1, 1 + cumsum(mi:1)[1:(mi - 1)]))
      if (include.diag) {
        sel <- 1:(mi * (mi + 1) / 2)
      }
      Ri <- vech(tcrossprod(yi))
      R <- c(R, Ri[sel])
    }
  }
  return(R)
}