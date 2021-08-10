library(nlme)
library(MASS)
library(refund)
library(matrixcalc)
library(mgcv)
library(Matrix)
library(Bolstad)
library(splines)

# functions
source("gen.data.R")
source("bootstrap.test.R")
source("direct.test.R")
source("multivariate.test.R")

stephanie_type1 <- function(seed = 2021087, k, n, m, L = 1000) {
    sim.success <- 0
    sim.stats <- c()
    l_time <- matrix(NA, nrow = k, ncol = 3)
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = "trigonometric", nsubj = n, r = 0, M = m)
        try(l_time[sim.success + 1, ] <- system.time(
            fit.b <- bootstrap.test(data, times, nbs = L))[1:3],
            silent = T)
        if ("try-error" %in% class(fit.b)) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, fit.b$p)
    }
    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        l_time)
}