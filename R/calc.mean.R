#######################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Calculates smooth mean using 10 thin plate regression splines
# Updated: Aug 4, 2018

calc.mean <- function(data) { # smooth mean
  gam0 <- mgcv::gam(as.vector(data$.value) ~ s(data$.index, k = 10))
  return(gam0$fitted.values)
}