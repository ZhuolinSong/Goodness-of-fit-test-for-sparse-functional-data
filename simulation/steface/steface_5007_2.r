library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

steface_5007_2 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type1(seed, s_k / 2, s_n, s_m, L = 1000, i_face = T)
}, mc.cores = 2)

save(steface_5007_2, file = "steface_5007_2.RData")