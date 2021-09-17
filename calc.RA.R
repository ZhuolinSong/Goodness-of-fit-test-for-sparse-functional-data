############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Calculates the Kronecker product of Y_i x Y_i for estimating alternative cov
# Note: Could be sped up if m_i equal for all subjects
# Updated: Aug 4, 2018

calc.RA <- function(data) {
  y <- data$.value
  subj <- data$.id
  usubj <- unique(subj)
  n <- length(usubj)
  R <- c()

  for (i in 1:n) {
    index <- which(subj == usubj[i])
    mi <- length(index)
    yi <- y[index]
    Ji <- matrix(1, mi, mi)
    diag(Ji) <- 0 # dropped diagonal
    di <- c(Ji)
    Ri <- (yi %x% yi)[which(di == 1)] # covariance (drop diagonal) of subject i
    R <- c(R, Ri)
  }
  return(Matrix(R))
}