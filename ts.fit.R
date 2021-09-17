############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Tensor-spline fit (using cubic B-splines) to data for smooth null and alternative covariance
# Updated: Aug 4, 2018

ts.fit <- function(data, times, H = 10) {
  p <- 3 ## cubic splines: degrees
  knots <- select.knots(seq(-1, 1, by = 0.01), H - p)
  # knots <- select.knots(seq(0,1,by=0.01),H-p)

  y <- data$.value
  t <- data$.index
  subj <- data$.id
  usubj <- unique(subj)
  n <- length(usubj)

  B <- matrix(, nrow = 0, ncol = H^2)
  Time <- matrix(, nrow = 0, ncol = 2)
  Y <- c()
  R <- c()

  for (i in 1:n) {
    index <- which(subj == usubj[i])
    mi <- length(index)
    ti <- t[index]
    yi <- y[index]

    Bi <- spline.des(knots = knots, x = ti, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
    Btildei <- Bi %x% Bi
    Ji <- matrix(1, mi, mi)
    diag(Ji) <- 0
    di <- c(Ji)
    Bbari <- Btildei[which(di == 1), ]
    T1i <- (ti %x% rep(1, mi))[which(di == 1)]
    T2i <- (rep(1, mi) %x% ti)[which(di == 1)]
    Ri <- (yi %x% yi)[which(di == 1)]

    B <- rbind(B, Bbari)
    Time <- rbind(Time, cbind(T1i, T2i))
    R <- c(R, Ri)
  }

  ## fine grid
  # B^*
  # st.construct <- function(tnew) {
  #   m1 <- length(tnew)
  #   if (m1 > 1) {
  #     st <- cbind(vech(kronecker(tnew, t(rep(1, m1)))),
  #                 vech(kronecker(rep(1, m1), t(tnew))))
  #   } else if (m1 == 1) {
  #     st <- rbind(st, c(tnew, tnew))
  #   }
  #   st
  # }
  # stnew <- st.construct(times)
  # Bnew1 <- spline.des(knots = knots, x = stnew[, 1], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  # Bnew2 <- spline.des(knots = knots, x = stnew[, 2], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  # Xstar <- Matrix(t(KhatriRao(Matrix(t(Bnew2)), Matrix(t(Bnew1)))))
  # delta_star <- which(stnew[, 1] != stnew[, 2])
  # Xstar[delta_star, ] <- sqrt(2) * Xstar[delta_star, ]

  Bstar <- spline.des(knots = knots, x = times, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  # temp1 <- t(Bstar) %*% Bstar
  # Eigen1 <- eigen(temp1)
  # A1 <- Eigen1$vectors %*% sqrt(diag(Eigen1$values)) %*% t(Eigen1$vectors)
  temp2 <- crossprod(B)
  Eigen2 <- eigen(temp2, symmetric = T)
  BtB.inv <- Matrix(Eigen2$vectors %*% tcrossprod(diag(1 / Eigen2$values), Eigen2$vectors))
  B.est <- tcrossprod(BtB.inv, B)
  return(list(B = B, Bstar = Bstar,
            # Xstar = Xstar,Bstar.tensor = Bstar %x% Bstar,
            Time = Time, R = Matrix(R),
            BtB.inv = BtB.inv, B.est = B.est))
}