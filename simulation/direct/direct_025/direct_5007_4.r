
library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 6000
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

direct_5007_4 <- mclapply(v_seed, seed_loop <- function(seed) {
    sim_direct(seed, s_k / 2, s_n, s_m, dev = "trigonometric", err = 0.25, r = 0)
}, mc.cores = 2)

save(direct_5007_4, file = "direct_5007_4_025.RData")