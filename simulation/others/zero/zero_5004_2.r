library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

zero_5004_2 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type1(seed, s_k / 2, s_n, s_m, L = 1000, i_face = T, lambda = 0)
}, mc.cores = 2)

save(zero_5004_2, file = "zero_5004_2.RData")