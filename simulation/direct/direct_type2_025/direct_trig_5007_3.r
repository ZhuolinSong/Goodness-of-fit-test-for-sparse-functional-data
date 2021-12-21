library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

s_seed <- 999983
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

direct_5007_3 <- mclapply(v_seed, seed_loop <- function(seed) {
    sim_direct(seed, s_k / 2, s_n, s_m, dev = "trigonometric", r =  r_grid_trig_500[3], err = 0.25)
}, mc.cores = 2)


save(direct_5007_3, file = "direct_trig_5007_3_025.RData")