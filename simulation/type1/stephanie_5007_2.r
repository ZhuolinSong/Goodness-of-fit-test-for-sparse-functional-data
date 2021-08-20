source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 7

stephanie_5007_2 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000)
save(stephanie_5007_2, file = "stephanie_5007_2.RData")