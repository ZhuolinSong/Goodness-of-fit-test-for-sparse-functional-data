source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983 + 6000
s_k <- 1000
s_n <- 500
s_m <- 7

stephanie_5007_4 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000)
save(stephanie_5007_4, file = "stephanie_5007_4.RData")