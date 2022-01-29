#######################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Example code for implementation of bootstrap, direct, and multivariate tests

# This code gives an example of generating data using the 'gen.data' function, based on
# the simulation study in the main paper. See the function for a description
# of inputs. The function outputs a data.frame of functional data that can be directly
# input into any of the testing function. The second example using the DTI dataset
# from the refund package.
#
# The functions 'bootstrap.test', 'direct.test', and 'multivariate.test' can be called
# to perform the corresponding test for the inputted dataset. Note that the multivariate'
# test can only be applied to dense (balanced) data where all subjects have the same
# time points. See the relevant function for information about changing default settings.
#
# Each testing function takes the two inputs inputs:
# 1) data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# 2) times: vector of all possible timepoints from -1 to 1, in sequential order
#
# Each testing function returns the null and alternative model fits, test statistic,
# and p-value. See the individual function pages for details.
#######################
#setwd("/Applications/Books/research/GEE/test covariance/R code_Oct24/R code")

devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
library(refund) 
data(grid)
data(ADNI)
data(bone)

# direct test(type 1)
sim_direct(seed = 2021087, k = 10, n = 100, m = 7, dev = "trigonometric", r = 0,
                mixed = T, err = 1)

# (power)
sim_direct(seed = 2021087, k = 10, n = 100, m = 7, dev = "quadratic", r = 0.5,
                mixed = T, err = 1)

# test sigma_error
sigma_error(seed = 2021087, k = 10, n = 100, m = 7, mixed = T, err = 1,
            approx = F, i_face = T, truncate.tn = 2)

# Type 1: Ftest(optimal)
stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1000,
              mixed = T, err = 1, approx = F, i_face = T, truncate.tn = 2)
# Type 1: Ftest(fixed_proximal) too slow
# stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1,
#              mixed = T, err = 1, approx = F, i_face = T, truncate.tn = 1, ic = 1)
# Type 1: Ftest(accel_proximal)
stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1000,
              mixed = T, err = 1, approx = F, i_face = T, truncate.tn = 1, ic = 2)
# Type 1: Ftest(accel_proximal_optimal)
stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1000,
              mixed = T, err = 1, approx = F, i_face = T, truncate.tn = 2, ic = 2)
# Type 1: Ftest(approx)
stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1000,
              mixed = T, err = 1, approx = T, i_face = T, truncate.tn = 1)

# Type 1: Original Test
stephanie_type1(seed = 2021087, k = 1, n = 100, m = 7, L = 1000,
              mixed = T, err = 1, approx = T, i_face = F, truncate.tn = 1)

# Type 2: Ftest(optimal)
stephanie_type2(seed = 2021087, k = 1, n = 100, m = 7,
              dev = "trigonometric", r = r_grid_quad[2], L = 10,
              mixed = T, err = 4, i_face = T, truncate.tn = 2, approx = F)
# Type 2: Ftest(approx)
stephanie_type2(seed = 2021087, k = 1, n = 500, m = 7,
              dev = "trigonometric", r = r_grid_quad_500[2], L = 10,
              mixed = T, err = 4, i_face = T, truncate.tn = 1, approx = T)
# Type 2: Original Test
stephanie_type2(seed = 2021087, k = 1, n = 100, m = 7,
              dev = "trigonometric", r = r_grid_quad[2], L = 10,
              mixed = T, err = 4, i_face = T, truncate.tn = 1, approx = T)

# Example 1. Simulated data from null model, with 100 subjects and 80 obs/subj
devtools::load_all()
library(refund)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- gen.data(deviation = "trigonometric", nsubj = 100, r = 0, M = 7, mixed_m = F)
times <- seq(-1, 1, length.out = 80) # all possible time points
m_cov_truth <- 1 + tcrossprod(times) - 0.5 * times - tcrossprod(rep(0.5, 80), times)

set.seed(2021085)
system.time(fit.b <- bootstrap.test(data, times, nbs = 10, nb = 10,
                      i_face = T, bs.mean = T, truncate.tn = 1, approx = T))

fit.b$bs.approx


# test PGD
nbs = 1000; nb = 10
mu <- calc.mean(data)
data.demean <- data.frame(.value = data$.value - mu, .index = data$.index, .id = data$.id)
fit.null <- fitNull(data.demean) # null fit
if ("try-error" %in% class(fit.null)) { # issue with null fit
  stop(fit.null)
}
b.fit <- ts.fit(data.demean, times = times, H = nb)
Rbar0.fit <- calc.R0(fit.null, b.fit$Time)

# nulls
Theta_nul_ap <- trunc.mat.theta(b.fit, Rbar0.fit$Rbar0, nb, 0)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_nul_ap), b.fit$Bstar)) - m_cov_truth, type = "F")
Theta_nul_pg <- fixed_PG(b.fit, Rbar0.fit$Rbar0, nb, alpha = vech(Theta_nul_ap), max_iter = 1e3)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_nul_pg), b.fit$Bstar)) - m_cov_truth, type = "F")
Theta_nul_ag <- A_PG(b.fit, Rbar0.fit$Rbar0, nb, alpha = vech(Theta_nul_ap), max_iter = 1e3)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_nul_ag), b.fit$Bstar)) - m_cov_truth, type = "F")

# alternative
Theta_alt_ap <- trunc.mat.theta(b.fit, b.fit$R, nb, 0)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_alt_ap), b.fit$Bstar)) - m_cov_truth, type = "F")
Theta_alt_pg <- fixed_PG(b.fit, b.fit$R, nb, alpha = vech(Theta_alt_ap), max_iter = 1e3, truncate.tn = 1)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_alt_pg), b.fit$Bstar)) - m_cov_truth, type = "F")
Theta_alt_ag <- A_PG(b.fit, b.fit$R, nb, alpha = vech(Theta_alt_ap), max_iter = 1e3, truncate.tn = 1)
norm(as.matrix(tcrossprod(b.fit$Bstar %*% Matrix(Theta_alt_ag), b.fit$Bstar)) - m_cov_truth, type = "F")


# Implement the tests
set.seed(2021085)
system.time(fit.b <- bootstrap.test(data, times, nbs = 10,
bs.mean = T, i_face = T, truncate.tn = 0, center = T))
fit.b$Theta.null
fit.b$Theta.alt

system.time(fit.b <- bootstrap.test(data, times, nbs = 10,
bs.mean = T, i_face = T, truncate.tn = 1, center = T))
fit.b$Theta.null
fit.b$Theta.alt

system.time(fit.b <- bootstrap.test(data, times, nbs = 10,
bs.mean = T, i_face = T, truncate.tn = 2, center = T))
fit.b$Theta.null
fit.b$Theta.alt


fit.b$p
fit.b$Tn
fit.b$bs.approx
norm(fit.b$C.null - m_cov_truth, type = "F")
norm(fit.b$C.alt - m_cov_truth, type = "F")
(fit.b$sigma2 - 1)^2

# fit.b<-bootstrap.test(data,times,nbs=1000) # full bootstrap test with 1000 resamples
fit.d <- direct.test(data, times) # direct test
fit.d$RLRT
fit.m <- multivariate.test(data, times) # multivariate test (note: data must be dense, ie M=80)



# Example 2. DTI data from "refund" package
nsub.full <- 382
times <- round(seq(-1, 1, length.out = 93), digits = 5)

data.full <- data.frame(.value = c(t(DTI$cca)), .index = rep(times, nsub.full), .id = rep(DTI$ID, each = 93), case = rep(DTI$case, each = 93), visit = rep(DTI$visit.time, each = 93))
data.full <- subset(data.full, case == 1 & visit == 0) # limit to patients w/ MS, baseline only
data.full <- data.full[complete.cases(data.full$.value), ] # no missing data

# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
new.subj <- 1
for (i in unique(data.full$.id)) {
  subj <- subset(data.full, .id == i)
  if (nrow(subj) < 93) {
    delete.subj <- c(delete.subj, i)
  }
  else {
    subj$.id <- new.subj
    data <- rbind(data, subj)
    new.subj <- new.subj + 1
  }
}

# Implement the tests
fit.b <- bootstrap.test(data, times, nbs = 10) # pilot bootstrap test with 10 resamples
# fit.b<-bootstrap.test(data,times,nbs=1000) # full bootstrap test with 1000 resamples
fit.d <- direct.test(data, times) # direct test
fit.m <- multivariate.test(data, times) # multivariate test (note: data must be dense)

# CD4 count
times <- round(seq(-1, 1, length.out = 61), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 1) {
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
length(unique(data$.index))
mean(count) #5.16
min(count) #1
max(count) #11

# No log-transform stepface
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- data.frame(.value = value, .index = index, .id = id)
### Ftest(optimal)
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.128, Tn = 0.08524647
fit.b1$Tn
### Ftest(approx)
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.158, Tn =  50052.47
fit.b2$Tn
### Original Test
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.145, Tn = 50052.47
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT #RLRT = 4.6571, p-value = 0.016

# Log-transform stepface
data <- data.frame(.value = log(value), .index = index, .id = id)
### Ftest(optimal)
fit.b4 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b4$p # p-value = 0.148, Tn = 0.08524647
fit.b4$Tn
### Ftest(approx)
fit.b5 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b5$p # p-value = 0.182, Tn = 0.1007515
fit.b5$Tn
### Original Test
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b6$p # p-value = 0.132, Tn = 0.1007515
fit.b6$Tn
### Direct Test
fit.d2 <- direct.test(data, times)
fit.d2$RLRT #RLRT = 1.9078, p-value = 0.0796


# CD4 count (with m >= 5)
times <- round(seq(-1, 1, length.out = 61), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 5) { # (with m >= 5)
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
mean(count) #6.7136
min(count) #5
max(count) #11
length(value) #1430
new.subj #214

# No log-transform stepface(with m >= 5)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- data.frame(.value = value, .index = index, .id = id)
### Ftest(optimal)(with m >= 5)
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.13, Tn = 38003.03
fit.b1$Tn
### Ftest(approx)(with m >= 5)
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.146, Tn =  52384.34
fit.b2$Tn
### Original Test(with m >= 5)
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.123, Tn = 52384.34
fit.b3$Tn
### Direct Test(with m >= 5)
fit.d1 <- direct.test(data, times)
fit.d1$RLRT #RLRT = 2.8929, p-value = 0.0404


# Log-transform stepface(with m >= 5)
data <- data.frame(.value = log(value), .index = index, .id = id)
### Ftest(optimal)(with m >= 5)
fit.b4 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b4$p # p-value = 0.158, Tn = 0.0785308
fit.b4$Tn
### Ftest(approx)(with m >= 5)
fit.b5 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b5$p # p-value = 0.163, Tn = 0.09779147
fit.b5$Tn
### Original Test(with m >= 5)
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b6$p # p-value = 0.15, Tn = 0.09779147
fit.b6$Tn
### Direct Test(with m >= 5)
fit.d2 <- direct.test(data, times)
fit.d2$RLRT #RLRT = 2.4082, p-value = 0.0593


#ADNI
times <- round(seq(-1, 1, length.out = 81), digits = 3)
load("data/ADNI.RData")
## ADAS_fd
str(ADAS_fd)
min(ADAS_fd["argvals"])
data <- data.frame(.value = ADAS_fd$y, .index = ADAS_fd$argvals*2 - 1, .id = ADAS_fd$subj)
str(data)
length(unique(ADAS_fd$argvals))
match(data$.index, times)
count <- NULL
for (i in seq_len(length(unique(data$.id)))) {
  subj.idx <- which(ADAS_fd$subj == i)
  subj.count <- length(subj.idx)
  count <- c(count, subj.count)
}
mean(count)
min(count)
max(count)


### Ftest(optimal)(with m >= 5)
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.909, Tn = 3265967
fit.b1$Tn
### Ftest(approx)(with m >= 5)
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.917, Tn = 3475345
fit.b2$Tn
### Original Test(with m >= 5)
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.643, Tn = 3519918
fit.b3$Tn
### Direct Test(with m >= 5)
fit.d4 <- direct.test(data, times)
fit.d4$RLRT #p-value = 1, RLRT = 0

##RAVLT.imme_fd
str(RAVLT.imme_fd)
min(RAVLT.imme_fd["argvals"])
max(RAVLT.imme_fd["argvals"])
data <- data.frame(.value = RAVLT.imme_fd$y, .index = RAVLT.imme_fd$argvals*2 - 1, .id = RAVLT.imme_fd$subj)
match(data$.index, times)

### Ftest(optimal)(with m >= 5)
fit.b5 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b5$p # p-value = 0.082, Tn = 512130420
fit.b5$Tn
### Ftest(approx)(with m >= 5)
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b6$p # p-value = 0.062, Tn = 680812477
fit.b6$Tn
### Original Test(with m >= 5)
fit.b7 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b7$p # p-value = 0.004, Tn = 680812477
fit.b7$Tn
### Direct Test(with m >= 5)
fit.d8 <- direct.test(data, times)
fit.d8$RLRT #p-value = 1, RLRT = 0
##Observed RLRT statistic is 0, no simulation performed.
##Warning message:
##In model.matrix.default(~m$groups[[n.levels - i + 1]] - 1, contrasts.arg = c("contr.treatment",  :
##  non-list contrasts argument ignored
##
##RAVLT.learn_fd
str(RAVLT.learn_fd)
min(RAVLT.learn_fd["argvals"])
max(RAVLT.learn_fd["argvals"])
data <- data.frame(.value = RAVLT.learn_fd$y, .index = RAVLT.learn_fd$argvals*2 - 1, .id = RAVLT.learn_fd$subj)
match(data$.index, times)

### Ftest(optimal)(with m >= 5)
fit.b9 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b9$p # p-value = 0.74, Tn = 535675.9
fit.b9$Tn
### Ftest(approx)(with m >= 5)
fit.b10 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b10$p # p-value = 0.755, Tn = 570370.6
fit.b10$Tn
### Original Test(with m >= 5)
fit.b11 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b11$p # p-value = 0.323, Tn = 570370.6
fit.b11$Tn
### Direct Test(with m >= 5)
fit.d12 <- direct.test(data, times)
fit.d12$RLRT #p-value = , RLRT = 

##FAQ_fd
str(FAQ_fd)
min(FAQ_fd["argvals"])
max(FAQ_fd["argvals"])
data <- data.frame(.value = FAQ_fd$y, .index = FAQ_fd$argvals*2 - 1, .id = FAQ_fd$subj)
match(data$.index, times)

### Ftest(optimal)(with m >= 5)
fit.b13 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b13$p # p-value = 0.453, RLRT = 26792298

fit.b13$Tn
### Ftest(approx)(with m >= 5)
fit.b14 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b14$p # p-value = 0.438, RLRT = 35521574
fit.b14$Tn
### Original Test(with m >= 5)
fit.b15 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b15$p # p-value = 0.254, RLRT = 35521574
fit.b15$Tn
### Direct Test(with m >= 5)
fit.d16 <- direct.test(data, times)
fit.d16$RLRT # p-value = , RLRT = 


##MMSE_fd
str(MMSE_fd)
min(MMSE_fd["argvals"])
max(MMSE_fd["argvals"])
data <- data.frame(.value = MMSE_fd$y, .index = MMSE_fd$argvals*2 - 1, .id = MMSE_fd$subj)
match(data$.index, times)

### Ftest(optimal)(with m >= 5)
fit.b17 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b17$p # p-value = 0.384, RLRT = 9295114
fit.b17$Tn
### Ftest(approx)(with m >= 5)
fit.b18 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b18$p # p-value = 0.398, RLRT = 12368271
fit.b18$Tn
### Original Test(with m >= 5)
fit.b19 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b19$p # p-value = 0.143, RLRT = 12368271
fit.b19$Tn
### Direct Test(with m >= 5)
fit.d20 <- direct.test(data, times)
fit.d20$RLRT # p-value = , RLRT = 


## spnbmd
str(bmd)
min(bmd["age"])
max(bmd["age"])
data <- data.frame(.value = bmd$spnbmd, .index = (bmd$age - 8.8)/(26.2-8.8)*2 - 1, .id = bmd$idnum)
min(data[".index"])
max(data[".index"])
times <- sort(c(round(seq(-1, 1, length.out = 81), digits = 3), unique(data$.index)))
match(data$.index, times)

count <- NULL
for (i in seq_len(max(unique(data$.id)))) {
  subj.idx <- which(data$.id == i)
  if (length(subj.idx) > 0) {
    subj.count <- length(subj.idx)
    count <- c(count, subj.count)
  }
}
mean(count)
min(count)
max(count)

### Ftest(optimal)(with m >= 5)
fit.b21 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b21$p # p-value = 0.977, RLRT = 500.7392
fit.b21$Tn
### Ftest(approx)(with m >= 5)
fit.b22 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b22$p # p-value = 0.99, RLRT = 484.7479
fit.b22$Tn
### Original Test(with m >= 5)
fit.b23 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b23$p # p-value = 0.989, RLRT = 484.7479
fit.b23$Tn
### Direct Test(with m >= 5)
fit.d24 <- direct.test(data, times)
fit.d24$RLRT # p-value = 0.4311, RLRT = 0.016717