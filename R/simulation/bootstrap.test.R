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

bootstrap.test <- function(data, times, nbs = 1000, nb = 10,
                           i_face = T, bs.mean = T, truncate.tn = 2, ...) {
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
  source("matrix.multiply.r")

  mu <- calc.mean(data)
  data.demean <- data.frame(.value = data$.value - mu, .index = data$.index, .id = data$.id)
  fit.null <- fitNull(data.demean) # null fit
  if ("try-error" %in% class(fit.null)) { # issue with null fit
    stop(fit.null)
  }
  b.fit <- ts.fit(data.demean, times = times, H = nb)
  Rbar0.fit <- calc.R0(fit.null, b.fit$Time)


  Theta.null <- trunc.mat.theta(b.fit, Rbar0.fit$Rbar0, nb, truncate.tn) # Smooth null cov
  Theta.alt <- trunc.mat.theta(b.fit, b.fit$R, nb, truncate.tn) # Smooth alt cov
  DX <- (Theta.null - Theta.alt) %*% b.fit$Xstar
  Tn <- sqrt(sum(DX * t(DX)))
  #print(Tn)
  C.alt <- as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta.alt), b.fit$Bstar))
  C.null <- as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta.null), b.fit$Bstar))
  #print(norm(C.alt - C.null, type = "F"))
  #DX <- trunc.mat(b.fit, Rbar0.fit$Rbar0, b.fit$R, nb, truncate.tn) %*% b.fit$Xstar
  #print(sqrt(sum(DX * t(DX))))
  # calculate sigma^2
  if (i_face) {
    data.sigsq <- data.frame(y = data$.value - mu, argvals = data$.index, subj = data$.id)
    sigsq <- (face.sparse(data.sigsq, ...))$sigma2 # false ?? or we need leave-one-subject-out mean nr??
  } else {
    sigsq <- calc.sigsq(data.demean, C.alt, times) # error var
  }
  # bootstrap
  bs.stats <- c()
  bs.success <- 0
  if (bs.mean) {
      mean.bs <- mu
  } else {
      mean.bs <- 0
  }
  data.demean.bs <- data

  while (bs.success < nbs) { # 0.07 per iteration
#ptm <- proc.time()
    ###### a. generate Y_ij^(l) (0.02s)
    y <- c(resample(data, mean.bs, Rbar0.fit$coef.null, sigsq))

    ###### b. center Y_ij^(l) (0.01s)
    r <- y
    if (bs.mean) {
      fit_mean.bs <- mgcv::gam(as.vector(y) ~ s(data$.index, k = nb))
      r <- y - fit_mean.bs$fitted.values
    }
    data.demean.bs$.value <- as.vector(r)


    ###### b. fitnull (0.03s)
    fit.null.bs <- fitNull(data.demean.bs) # null fit (slow here I think)
    if ("try-error" %in% class(fit.null.bs)) { # issue with null fit
      next # if problem
    }
    Rbar0 <- calc.R0(fit.null.bs, b.fit$Time)$Rbar0

    ###### c. Initialize R^(a) (0.00s)
    RbarA <- calc.RA(data.demean.bs) # R for alt

    ###### d. calculate Tn.bs (0.0s)
    Delta.bs <- trunc.mat(b.fit, Rbar0, RbarA, nb, truncate.tn) # slow here

    DX.bs <- Delta.bs %*% b.fit$Xstar
    Tn.bs <- sqrt(sum(DX.bs * t(DX.bs)))
    bs.stats <- c(bs.stats, Tn.bs) # save bs stats
    bs.success <- bs.success + 1
#print(proc.time() - ptm)
  }

  Tn.stats <- p.bs(Tn, unlist(bs.stats))
  list(
    mu = mu,
    C.alt = C.alt,
    C.null = C.null,
    sigma2 = sigsq,
    Tn = Tn, p = Tn.stats$p, p.var = Tn.stats$var,
    bs.approx = bs.stats
  )
}