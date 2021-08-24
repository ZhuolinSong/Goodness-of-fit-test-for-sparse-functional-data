library(nlme)
library(MASS)
library(refund)
library(matrixcalc)
library(mgcv)
library(Matrix)
library(Bolstad)
library(splines)
library(face)


# functions
source("gen.data.R")
source("bootstrap.test.R")
source("direct.test.R")
source("multivariate.test.R")

stephanie_type1 <- function(seed = 2021087, k, n, m, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    sim.c0 <- c()
    sim.calt <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    cov_truth <- 1 + tcrossprod(times) - 0.5 * times - 0.5 * matrix(rep(times, 80), 80, byrow = T)

    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = "trigonometric", nsubj = n, r = 0, M = m)
        l_time[[sim.success + 1]] <- try(system.time(
            fit.b <- bootstrap.test(data, times, nbs = L, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(l_time[[sim.success + 1]])) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, fit.b$p)
        sim.sigma2 <- c(sim.sigma2, fit.b$sigma2)
        sim.c0 <- c(sim.c0, norm(fit.b$C.null - cov_truth, type = "F"))
        sim.calt <- c(sim.calt, norm(fit.b$C.alt - cov_truth, type = "F"))
    }
    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        mean((sim.sigma2 - 1)^2),
        mean(sim.c0),
        mean(sim.calt),
        rowMeans(matrix(unlist(l_time), nrow = 3)),
        c(seed, sim.success))
}