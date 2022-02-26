library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 100
s_m <- 7

new_plain_quad_1007 <-  mclapply(new_r_grid_quad, r_loop <- function(s_r) {
    stephanie_type2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000, i_face = F, approx = T, truncate.tn = 1)
}, mc.cores = 2)

save(new_plain_quad_1007, file = "new_plain_quad_1007.RData")