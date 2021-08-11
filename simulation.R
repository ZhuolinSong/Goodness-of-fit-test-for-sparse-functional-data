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
    sim.sigma2 <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = "trigonometric", nsubj = n, r = 0, M = m)
        l_time[[sim.success + 1]] <- try(system.time(
            fit.b <- bootstrap.test(data, times, nbs = L))[1:3],
            silent = T)
        if ("try-error" %in% class(l_time[[sim.success + 1]])) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, fit.b$p)
        sim.sigma2 <- c(sim.sigma2, fit.b$sigma2)
    }
    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        sum((sim.sigma2 - 1)^2),
        l_time)
}