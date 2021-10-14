############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Conduct the direct test for testing quadratic polynomial covariance

# Inputs
# data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# times: vector of all possible timepoints from -1 to 1, in sequentially order

# Outputs
# mu: Estimated mean subtracted from data
# fit.alt: Alternative model fit (linear random effects + quadratic term)
# fit.null: Null model fit (linear random effects)
# fit.quad: Model fit with just quadratic term
# RLRT: Results for restricted-likelihood ratio test of quadratic term

direct.test<-function(data,times){
  # libraries
  library(RLRsim)
  library(matrixcalc)
  library(nlme)
  library(mgcv)
  
  #functions
  calc.mean.direct<-function(data){ #smooth mean
    gam0<-gam(as.vector(data$.value)~s(data$.index,k=10))
    return(gam0$fitted.values)
  }
  fit.alt.direct<-function(data,nl,nk){
    try(lme(.value~1,random=list(.id = pdIdent(~.indexsq-1),
                                 .id = pdSymm(~1+.index)),data=data),silent=TRUE)
    
  }
  fit.null.direct<-function(data,nl,nk){
    try(lme(.value~1,random=list(.id=pdSymm(~1+.index)),data=data),silent=TRUE)
  }
  fit.quad.direct<-function(data,nl,nk){
    try(lme(.value~1,random=list(.id = pdIdent(~.indexsq-1)),data=data),silent=TRUE)
  }
  
  mu<-calc.mean.direct(data)
  data$.value<-data$.value-mu
  data$.indexsq<-data$.index^2
    
  alt.fit<-fit.alt.direct(data)
  null.fit<-fit.null.direct(data)
  quad.fit<-fit.quad.direct(data)
  if('try-error' %in% class(alt.fit)){
    return('Failure in alternative model fit')
  }
  if('try-error' %in% class(null.fit)){
    return('Failure in null model fit')
  }
  if('try-error' %in% class(quad.fit)){
    return('Failure in quadratic-only model fit')
  }
  RLRT<-exactRLRT(quad.fit,alt.fit,null.fit)
  list(mu=mu,alt.fit=alt.fit,null.fit=null.fit,quad.fit=quad.fit,RLRT=RLRT)
}
