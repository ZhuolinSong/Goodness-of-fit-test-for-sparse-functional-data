library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 6000
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

code_5004_4 <- mclapply(v_seed, seed_loop <- function(seed) {
    stephanie_type1(seed, s_k / 2, s_n, s_m, L = 1000, approx = T, i_face = T, err = 1, truncate.tn = 1)
}, mc.cores = 2)

save(code_5004_4, file = "code_5004_4_4.RData")