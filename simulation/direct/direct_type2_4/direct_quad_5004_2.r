library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

s_seed <- 999983
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

direct_5004_2 <- mclapply(v_seed, seed_loop <- function(seed) {
    sim_direct(seed, s_k / 2, s_n, s_m, dev = "quadratic", r =  r_grid_quad_500[2], err = 4)
}, mc.cores = 2)


save(direct_5004_2, file = "direct_quad_5004_2_4.RData")