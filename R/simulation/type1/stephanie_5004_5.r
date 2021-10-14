library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 8000
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

stephanie_5004_5 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type1(seed, s_k / 2, s_n, s_m, L = 1000)
}, mc.cores = 2)

save(stephanie_5004_5, file = "stephanie_5004_5.RData")