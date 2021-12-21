library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

orig_1007 <- mclapply(v_seed, seed_loop <- function(seed) {
    sigma_error(seed, s_k / 2, s_n, s_m, i_face = F, err = 1, approx = T, truncate.tn = 1)
}, mc.cores = 2)

save(orig_1007, file = "orig_1007.RData")