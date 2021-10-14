library(devtools)
library(parallel)
source("simulation.R")
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 100
s_m <- 7

steface_trig_1007 <- mclapply(r_grid_trig[-1], r_loop <- function(s_r) {
    stephanie_type2(seed, s_k, s_n, s_m, "trigonometric", s_r, L = 1000)
}, mc.cores = 2)

save(steface_trig_1007, file = "steface_trig_1007.RData")