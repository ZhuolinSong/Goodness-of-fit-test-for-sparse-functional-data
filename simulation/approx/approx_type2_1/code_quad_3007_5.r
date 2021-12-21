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

code_3007_5 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type2(seed, s_k / 2, s_n, s_m, "quadratic", r_grid_quad[5], L = 1000, i_face = T, approx = T, truncate.tn = 1)
}, mc.cores = 2)


save(code_3007_5, file = "code_quad_3007_5.RData")