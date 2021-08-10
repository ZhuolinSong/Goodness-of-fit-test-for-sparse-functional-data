############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Fit the null model (linear random effects) to data
# Updated: Aug 4, 2018

fitNull <- function(data) {
  try(lme(.value ~ 1, random = list(.id = pdSymm(~ 1 + .index)), data = data), silent = T)
}