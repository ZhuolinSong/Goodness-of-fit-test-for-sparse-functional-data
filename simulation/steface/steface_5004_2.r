source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 4

stephanie_5004_2 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000, i_face = T)
save(stephanie_5004_2, file = "stephanie_5004_2.RData")