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

plain_5004_5 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type2(seed, s_k / 2, s_n, s_m, "quadratic", r_grid_quad_500[5], L = 1000, err = 4, i_face = F, approx = T, truncate.tn = 1)
}, mc.cores = 2)


save(plain_5004_5, file = "plain_quad_5004_5.RData")