library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 4000
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

orig_5004_3 <- mclapply(v_seed, seed_loop <- function(seed) {
    sigma_error(seed, s_k / 2, s_n, s_m, i_face = F, err = 1, approx = T, truncate.tn = 1)
}, mc.cores = 2)

save(orig_5004_3, file = "orig_5004_3.RData")