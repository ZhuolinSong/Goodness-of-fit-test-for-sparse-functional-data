library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 100
s_m <- 4

code_trig_1004 <- mclapply(r_grid_trig[-1], r_loop <- function(s_r) {
    stephanie_type2(seed, s_k, s_n, s_m, "trigonometric", s_r, L = 1000, err = 0.25, i_face = T, approx = T, truncate.tn = 1)
}, mc.cores = 2)

save(code_trig_1004, file = "code_trig_1004.RData")