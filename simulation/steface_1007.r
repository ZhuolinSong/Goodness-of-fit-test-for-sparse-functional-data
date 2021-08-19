source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 7

steface_1007 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000, i_face = T)
save(steface_1007, file = "steface_1007.RData")