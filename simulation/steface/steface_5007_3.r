source("simulation.R")
RNGkind("L'Ecuyer-CMRG")

seed <- 999983 + 4000
s_k <- 1000
s_n <- 500
s_m <- 7

stephanie_5007_3 <- stephanie_type1(seed, s_k, s_n, s_m, L = 1000, i_face = T)
save(stephanie_5007_3, file = "stephanie_5007_3.RData")