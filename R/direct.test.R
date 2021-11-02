############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Conduct the direct test for testing quadratic polynomial covariance

# Inputs
# data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# times: vector of all possible timepoints from -1 to 1, in sequentially order

# Outputs
# mu: Estimated mean subtracted from data
# fit.alt: Alternative model fit (linear random effects + quadratic term)
# fit.null: Null model fit (linear random effects)
# fit.quad: Model fit with just quadratic term
# RLRT: Results for restricted-likelihood ratio test of quadratic term
# Outputs
# fit.alt: Alternative model fit (functional principal components analysis)
# fit.null: Null model fit (linear random effects)
# C.alt: Covariance matrix under alternative model
# C.null: Covariance matrix under null model
# tn: Test statistic
# p: p-value for test statistic based on the bs.approx
# bs.approx: List of values from the null distribution of Tn
#######################

#' @param data  "data frame with three arguments:
#'                  (1) "argvals": observation times;
#'                  (2) "subj": subject indices;
#'                  (3) "y": values of observations;
#'  Note that: we only handle complete data, so missing values are not allowed at this moment
#' @param times  "argvals.new" if we want the estimated covariance function at "argvals.new"; if NULL,

#' @import RLRsim
#' @import matrixcalc
#' @import mgcv
#' @import nlme
direct.test <- function(data, times) {

  # functions
  calc.mean.direct <- function(data) { # smooth mean
    gam0 <- gam(as.vector(data$.value) ~ s(data$.index, k = 10))
    return(gam0$fitted.values)
  }
  fit.alt.direct <- function(data, nl, nk) {
    try(lme(.value ~ 1, random = list(
      .id = pdIdent(~ .indexsq - 1),
      .id = pdSymm(~ 1 + .index)
    ), data = data), silent = TRUE)
  }
  fit.null.direct <- function(data, nl, nk) {
    try(lme(.value ~ 1, random = list(.id = pdSymm(~ 1 + .index)), data = data), silent = TRUE)
  }
  fit.quad.direct <- function(data, nl, nk) {
    try(lme(.value ~ 1, random = list(.id = pdIdent(~ .indexsq - 1)), data = data), silent = TRUE)
  }

  mu <- calc.mean.direct(data)
  data$.value <- data$.value - mu
  data$.indexsq <- data$.index^2

  alt.fit <- fit.alt.direct(data)
  null.fit <- fit.null.direct(data)
  quad.fit <- fit.quad.direct(data)
  if ("try-error" %in% class(alt.fit)) {
    return("Failure in alternative model fit")
  }
  if ("try-error" %in% class(null.fit)) {
    return("Failure in null model fit")
  }
  if ("try-error" %in% class(quad.fit)) {
    return("Failure in quadratic-only model fit")
  }
  RLRT <- exactRLRT(quad.fit, alt.fit, null.fit)
  list(mu = mu, alt.fit = alt.fit, null.fit = null.fit, quad.fit = quad.fit, RLRT = RLRT)
}