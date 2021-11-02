library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 5
v_seed <- c(s_seed, s_seed + s_k)

stephanie_1005 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type1(seed, s_k / 2, s_n, s_m, L = 1000)
}, mc.cores = 2)

save(stephanie_1005, file = "stephanie_1005.RData")