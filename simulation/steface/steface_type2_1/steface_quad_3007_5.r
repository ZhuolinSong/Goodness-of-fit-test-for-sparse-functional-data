library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

s_seed <- 999983
s_k <- 1000
s_n <- 300
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

steface_3007_5 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type2(seed, s_k / 2, s_n, s_m, "quadratic", r_grid_quad[5])
}, mc.cores = 2)


save(steface_3007_5, file = "steface_quad_3007_5.RData")