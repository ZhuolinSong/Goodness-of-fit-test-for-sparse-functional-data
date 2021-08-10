source("simulation.R")


RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 500
s_m <- 5

stephanie_5005 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000)
save(stephanie_5005, file = "stephanie_5005.RData")