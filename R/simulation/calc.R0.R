############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Calculates null covariance estimate at all observed time pairs, for estimating
#          smooth null covariance
# Updated: Aug 4, 2018

calc.R0 <- function(fit.null, times) {
  var.mat <- VarCorr(fit.null)
  sigsq0 <- as.numeric(var.mat[1, 1])
  sigsq1 <- as.numeric(var.mat[2, 1])
  cov01 <- as.numeric(var.mat[2, 3]) * sqrt(sigsq0 * sigsq1) # corr->cov
  Rbar0 <- sigsq0 + cov01 * (times[, 1] + times[, 2]) + sigsq1 * (times[, 1] * times[, 2])
  list(Rbar0 = Rbar0, coef.null = c(sigsq0, cov01, sigsq1))
}