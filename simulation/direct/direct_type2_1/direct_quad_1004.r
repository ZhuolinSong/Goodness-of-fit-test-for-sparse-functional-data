library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 100
s_m <- 4

direct_quad_1004 <-  mclapply(r_grid_quad[-1], r_loop <- function(s_r) {
    sim_direct(seed, s_k, s_n, s_m, dev = "quadratic", r = s_r)
}, mc.cores = 2)

save(direct_quad_1004, file = "direct_quad_1004.RData")