#######################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Apply the bootstrap test for testing a quadratic polynomial covariance
# Updated: Aug 4, 2018

# Inputs
# data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# times: vector of all possible timepoints from -1 to 1, in sequential order
# nbs: number of bootstrap samples, default = 1000
# nb: number of cubic B-splines per axis, for the FPCA alternative model fit, default = 10

# Outputs
# fit.alt: Alternative model fit (functional principal components analysis)
# fit.null: Null model fit (linear random effects)
# C.alt: Covariance matrix under alternative model
# C.null: Covariance matrix under null model
# tn: Test statistic
# p: p-value for test statistic based on the bs.approx
# bs.approx: List of values from the null distribution of Tn
#######################

bootstrap.test <- function(data, times, nbs = 1000, nb = 10) {
  source("calc.mean.R")
  source("select.knots.R")
  source("trunc.mat.R")
  source("ts.fit.R")
  source("calc.RA.R")
  source("calc.sigsq.R")
  source("fitNull.R")
  source("calc.R0.R")
  source("irreg2mat.mod.R")
  source("resample.R")
  source("p.bs.R")

  mu <- calc.mean(data)
  data.demean <- data.frame(.value = data$.value - mu, .index = data$.index, .id = data$.id)
  fit.null <- fitNull(data.demean) # null fit
  if ("try-error" %in% class(fit.null)) { # issue with null fit
    stop(fit.null)
  }
  b.fit <- ts.fit(data.demean, times = times, H = nb)

  #pre compute
  # m_est <- b.fit$Bstar.tensor %*% (b.fit$BtB.inv %*% t(b.fit$B) )

  C.alt <- trunc.mat(b.fit, b.fit$R, times) # Smooth alt cov
  sigsq <- calc.sigsq(data.demean, C.alt, times) # error var

  Rbar0.fit <- calc.R0(fit.null, b.fit$Time)
  C.null <- trunc.mat(b.fit, Rbar0.fit$Rbar0, times) # Smooth null cov
  Tn <- norm(C.alt - C.null, type = "F")

  # bootstrap
  bs.stats <- c()
  bs.success <- 0
  while (bs.success < nbs) {
    this.bs <- resample(data, mu, Rbar0.fit$coef.null, sigsq)
    mu.bs <- calc.mean(this.bs)
    data.demean.bs <- data.frame(.value = this.bs$.value - mu.bs, .index = this.bs$.index, .id = this.bs$.id)
    fit.null.bs <- fitNull(data.demean.bs) # null fit
    if ("try-error" %in% class(fit.null.bs)) { # issue with null fit
      next # if problem
    }
    RbarA <- calc.RA(data.demean.bs) # R for alt
    Rbar0.fit.bs <- calc.R0(fit.null.bs, b.fit$Time)
    C.alt.bs <- trunc.mat(b.fit, RbarA, times) # slow here
    C.null.bs <- trunc.mat(b.fit, Rbar0.fit.bs$Rbar0, times) # slow here
    Tn.bs <- norm(C.alt.bs - C.null.bs, type = "F")
    bs.stats <- c(bs.stats, Tn.bs) # save bs stats
    bs.success <- bs.success + 1
  }
  Tn.stats <- p.bs(Tn, unlist(bs.stats))
  list(mu = mu, C.alt = C.alt, C.null = C.null, sigma2 = sigsq, Tn = Tn, p = Tn.stats$p, bs.approx = bs.stats)
}