source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 500
s_m <- 6

stephanie_5006 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000)
save(stephanie_5006, file = "stephanie_5006.RData")