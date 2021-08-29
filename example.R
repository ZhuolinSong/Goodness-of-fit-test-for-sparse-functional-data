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

source("simulation.R")

# Example 1. Simulated data from null model, with 100 subjects and 80 obs/subj
data <- gen.data(deviation = "trigonometric", nsubj = 100, r = 0, M = 5, mixed_m = T)
times <- seq(-1, 1, length.out = 80) # all possible time points
m_cov_truth <- 1 + tcrossprod(times) - 0.5 * times - tcrossprod(rep(0.5, 80), times)
set.seed(2021083)
# Implement the tests
system.time(fit.b <- bootstrap.test(data, times, nbs = 1)) # pilot bootstrap test with 10 resamples

fit.b$p
norm(fit.b$C.null - m_cov_truth, type = "F")
norm(fit.b$C.alt - m_cov_truth, type = "F")


# fit.b<-bootstrap.test(data,times,nbs=1000) # full bootstrap test with 1000 resamples
fit.d <- direct.test(data, times) # direct test
fit.m <- multivariate.test(data, times) # multivariate test (note: data must be dense, ie M=80)

stephanie_type1(seed = 2021087, k = 5, n = 100, m = 5, L = 1,
                mixed = T, i_face = T, face.mean = F)

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