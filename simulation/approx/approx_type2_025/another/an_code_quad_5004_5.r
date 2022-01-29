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

an_code_5004_5 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type2(seed, s_k / 2, s_n, s_m, "quadratic", an_r_grid_quad_500[4], L = 1000, err = 0.25, i_face = T, approx = T, truncate.tn = 1)
}, mc.cores = 2)


save(an_code_5004_5, file = "an_code_quad_5004_5.RData")