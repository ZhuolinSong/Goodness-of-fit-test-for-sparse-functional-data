library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

direct_1007 <- mclapply(v_seed, seed_loop <- function(seed) {
    sim_direct(seed, s_k / 2, s_n, s_m, dev = "trigonometric", r = 0)
}, mc.cores = 2)

save(direct_1007, file = "direct_1007.RData")