library(devtools)
library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 500
s_m <- 7

steface_quad_5007 <-  mclapply(r_grid_quad[-1], r_loop <- function(s_r) {
    stephanie_type2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000)
}, mc.cores = 4)

save(steface_quad_5007, file = "steface_quad_5007.RData")