
stephanie_type1 <- function(seed = 2021087, k, n, m, L = 1000,
                mixed = T, err = 1, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    #sim.c0 <- c()
    #sim.calt <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    #cov_truth <- 1 + tcrossprod(times) - 0.5 * times - 0.5 * matrix(rep(times, 80), 80, byrow = T)

    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(nsubj = n, r = 0, M = m, error.var = err, mixed_m = mixed)
        l_time[[sim.success + 1]] <- try(system.time(
            fit.b <- bootstrap.test(data, times, nbs = L, ...))[1:3],
            silent = T)
        # fit.b <- try(bootstrap.test(data, times, nbs = L, ...), silent = T)
        if ("try-error" %in% class(fit.b)) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, fit.b$p)
        sim.sigma2 <- c(sim.sigma2, fit.b$sigma2)
        #sim.c0 <- c(sim.c0, norm(fit.b$C.null - cov_truth, type = "F"))
        #sim.calt <- c(sim.calt, norm(fit.b$C.alt - cov_truth, type = "F"))
    }
    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        mean((sim.sigma2 - err)^2),
        #mean(sim.c0),
        #mean(sim.calt),
        rowMeans(matrix(unlist(l_time), nrow = 3)),
        c(seed, sim.success),
        sim.stats)
}

stephanie_type2 <- function(seed = 2021087, k, n, m, dev, r, L = 1000,
            mixed = T, err = 1, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    #sim.c0 <- c()
    #sim.calt <- c()
    #l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    #cov_truth <- 1 + tcrossprod(times) - 0.5 * times - 0.5 * matrix(rep(times, 80), 80, byrow = T)

    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m, error.var = err, mixed_m = mixed)
        # l_time[[sim.success + 1]] <- try(system.time(
        #     fit.b <- bootstrap.test(data, times, nbs = L, ...))[1:3],
        #     silent = T)
        fit.b <- try(bootstrap.test(data, times, nbs = L, ...), silent = T)
        if ("try-error" %in% class(fit.b)) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, fit.b$p)
        sim.sigma2 <- c(sim.sigma2, fit.b$sigma2)
        #sim.c0 <- c(sim.c0, norm(fit.b$C.null - cov_truth, type = "F"))
        #sim.calt <- c(sim.calt, norm(fit.b$C.alt - cov_truth, type = "F"))
    }
    list(mean(sim.stats <= 0.05),
        mean((sim.sigma2 - err)^2),
        #mean(sim.c0),
        #mean(sim.calt),
        #rowMeans(matrix(unlist(l_time), nrow = 3)),
        c(seed, sim.success),
        sim.stats)
}