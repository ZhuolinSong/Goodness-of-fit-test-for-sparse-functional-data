############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Tensor-spline fit (using cubic B-splines) to data for smooth null and alternative covariance
# Updated: Aug 4, 2018

ts.fit <- function(data, times, H = 10, include.diag = F) {
  p <- 3 ## cubic splines: degrees
  knots <- select.knots(seq(-1, 1, by = 0.01), H - p)
  # knots <- select.knots(seq(0,1,by=0.01),H-p)

  y <- data$.value
  t <- data$.index
  subj <- data$.id
  usubj <- unique(subj)
  n <- length(usubj)

  Time <- matrix(, nrow = 0, ncol = 2)
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

  c <- dim(B1)[2]
  G <- Matrix(duplication.matrix(c))
  BG <- B %*% G

  Bstar <- spline.des(knots = knots, x = times, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  # temp1 <- t(Bstar) %*% Bstar
  # Eigen1 <- eigen(temp1)
  # A1 <- Eigen1$vectors %*% sqrt(diag(Eigen1$values)) %*% t(Eigen1$vectors)
  temp2 <- crossprod(BG)
  Eigen2 <- eigen(temp2, symmetric = T)
  BtB.inv <- Matrix(Eigen2$vectors %*% tcrossprod(diag(1 / Eigen2$values), Eigen2$vectors))
  B.est <- tcrossprod(BtB.inv, BG)
  return(list(B = B, Bstar = Bstar, Xstar = crossprod(Bstar),
            G = G, Time = Time, R = R,
            BtB.inv = BtB.inv, B.est = B.est))
}