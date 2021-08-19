source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 500
s_m <- 7

steface_5007 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000, i_face = T)
save(steface_5007, file = "steface_5007.RData")