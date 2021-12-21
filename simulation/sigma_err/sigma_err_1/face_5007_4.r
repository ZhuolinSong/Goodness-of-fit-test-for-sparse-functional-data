library(parallel)
library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 6000
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

face_5007_4 <- mclapply(v_seed, seed_loop <- function(seed) {
    sigma_error(seed, s_k / 2, s_n, s_m, i_face = T, err = 1)
}, mc.cores = 2)

save(face_5007_4, file = "face_5007_4.RData")