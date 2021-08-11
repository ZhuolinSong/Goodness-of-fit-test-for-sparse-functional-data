source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 6

stephanie_1006 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000)
save(stephanie_1006, file = "stephanie_1006.RData")