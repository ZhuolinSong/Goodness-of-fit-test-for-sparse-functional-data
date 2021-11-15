############################
#' bootstrap.face
#'
#' Apply the bootstrap test for testing a quadratic polynomial covariance
#'
#' @param data  "data frame with three arguments:
#'                  (1) "argvals": observation times;
#'                  (2) "subj": subject indices;
#'                  (3) "y": values of observations;
#'  Note that: we only handle complete data, so missing values are not allowed at this moment
#' @param nbs number of bootstrap samples, default = 1000
#' @param times  "argvals.new" if we want the estimated covariance function at "argvals.new"; if NULL,
#' then 100 equidistant points in the range of "argvals" in "data"
#' @param bs.mean   "center" means if we want to compute population mean
#' @param nb   number of interior knots for B-spline basis functions to be used;
#'
#' @import matrixcalc
#' @import Matrix
#' @import splines
#' @return an object "bootstrap.face" contain:
#'  fit.alt: Alternative model fit (functional principal components analysis)
#'  fit.null: Null model fit (linear random effects)
#'  C.alt: Covariance matrix under alternative model
#'  C.null: Covariance matrix under null model
#'  tn: Test statistic
#'  p: p-value for test statistic based on the bs.approx
#'  bs.approx: List of values from the null distribution of Tn
#'
#' @references modified from bootstrap.test.R written by Stephanie

ts.fit <- function(data, times, H = 10, include.diag = F, m = 8e5) {
  p <- 3 ## cubic splines: degrees
  knots <- select.knots(seq(-1, 1, by = 0.01), H - p)
  # knots <- select.knots(seq(0,1,by=0.01),H-p)

  y <- data$.value
  t <- data$.index
  subj <- data$.id
  usubj <- unique(subj)
  n <- length(usubj)

  Time <- matrix(NA, nrow = 0, ncol = 2)
  R <- c()

  for (i in 1:n) {
    index <- which(subj == usubj[i])
    mi <- length(index)
    ti <- t[index]
    yi <- y[index]

    if (mi > 1) {
      # <------ select off diagonal only
      sel <- setdiff(1:(mi * (mi + 1) / 2), c(1, 1 + cumsum(mi:1)[1:(mi - 1)]))
      if (include.diag) {
        sel <- 1:(mi * (mi + 1) / 2)
      }

      T1i <- vech(ti %x% t(rep(1, mi)))
      T2i <- vech(rep(1, mi) %x% t(ti))
      Ri <- vech(tcrossprod(yi))

      Time <- rbind(Time, cbind(T1i, T2i)[sel, ])
      R <- c(R, Ri[sel])
    }
  }


  B1 <- spline.des(knots = knots, x = Time[, 1], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  B2 <- spline.des(knots = knots, x = Time[, 2], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  B <- Matrix(t(KhatriRao(Matrix(t(B2)), Matrix(t(B1)))))

#  B <- matrix(, nrow = 0, ncol = H^2)
#  for (i in 1:n) {
#    index <- which(subj == usubj[i])
#    mi <- length(index)
#    ti <- t[index]
#    yi <- y[index]
#
#    Bi <- spline.des(knots = knots, x = ti, ord = p + 1, outer.ok = TRUE)$design
#    Btildei <- Bi %x% Bi
#    Ji <- matrix(1, mi, mi)
#    diag(Ji) <- 0
#    di <- c(Ji)
#    Bbari <- Btildei[which(di == 1), ]
#    T1i <- (ti %x% rep(1, mi))[which(di == 1)]
#    T2i <- (rep(1, mi) %x% ti)[which(di == 1)]
#    Ri <- (yi %x% yi)[which(di == 1)]
#
#    B <- rbind(B, Bbari)
#    Time <- rbind(Time, cbind(T1i, T2i))
#    R <- c(R, Ri)
#  }

  # c <- dim(B1)[2]
  G <- Matrix(duplication.matrix(H))
  BG <- B %*% G

  Bstar <- spline.des(knots = knots, x = times, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  # temp1 <- t(Bstar) %*% Bstar
  # Eigen1 <- eigen(temp1)
  # A1 <- Eigen1$vectors %*% sqrt(diag(Eigen1$values)) %*% t(Eigen1$vectors)
  BtB <- crossprod(BG)
  Eigen2 <- eigen(BtB, symmetric = T)
  BtB.inv <- Matrix(Eigen2$vectors %*% tcrossprod(diag(1 / Eigen2$values), Eigen2$vectors))
  B.est <- tcrossprod(BtB.inv, BG)
  dense_times <- seq(min(times), max(times), length.out = m)
  Xstar <- spline.des(knots = knots, x = dense_times, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  Xstar <- crossprod(Xstar) / m
  Eigen1 <- eigen(Xstar, symmetric = T)
  Xstar.half <- Eigen1$vectors %*% diag(sqrt(Eigen1$values)) %*% t(Eigen1$vectors)
  Xstar.invhalf <- Eigen1$vectors %*% diag(1 / sqrt(Eigen1$values)) %*% t(Eigen1$vectors)
  return(list(B = BG, Bstar = Bstar,
            Xstar = Xstar, Xstar.half = Xstar.half, Xstar.invhalf = Xstar.invhalf,
            G = G, Time = Time, R = R,
            BtB.inv = BtB.inv, B.est = B.est,
            BtB = BtB))
}