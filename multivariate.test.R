########################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Apply the multivariate test from Zhong et al. (2017)
# for testing a quadratic polynomial covariance

# Inputs
# data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# times: vector of all possible timepoints from -1 to 1, in sequentially order

# Outputs
# C.alt: Estimated alternative covariance matrix (unstructured)
# C.null: Estimated null covariance matrix (quadratic polynomial)
# Lambdan: Test statistic
# Tn: Unbiased estimator of Frobenius difference between null and alternative covariances
# Jn3: Adjustment for error in parameter estimation
# z.stat: Z statistic from asymptotic normal null distribution
# p.z: P-value for z statistic
# chi.stat: Chi-squared statistic from fixed-sample null distribution
# p.chi: P-value from chi-squared statistic
# chi.samp: Approximation for fixed-sample null distribution
########################

multivariate.test<-function(data,times){
  # libraries
  library(mgcv)

  # functions
  calc.mean<-function(data){ #smooth mean
    gam0<-gam(as.vector(data$.value)~s(data$.index,k=10))
    return(gam0$fitted.values)
  }
  tr<-function(A){
    sum(diag(A))
  }
  calc.dist<-function(sigma.est){
    Sigmat0.est<-sigma.est[1]^2*ones%*%t(ones)+sigma.est[2]*(ones%*%t(times)+times%*%t(ones))+sigma.est[3]^2*times%*%t(times)+sigma.est[4]^2*diag(p)
    tr((Sigmat1-Sigmat0.est)%*%(Sigmat1-Sigmat0.est))
  }
  
  p=length(times)
  n=length(unique(data$.id))
  ones<-rep(1,p)
    
  mu<-calc.mean(data)
  data.demean<-data.frame(.value=data$.value-mu,data[,2:3]) #demean data
    
  epsilon.mat<-matrix(data.demean$.value,ncol=p,byrow=T) #matrix of residuals, each row is a subj
  Sigmat1<-t(epsilon.mat)%*%epsilon.mat/n #Unstructured Cov
  sig.est<-optim(par=c(1,-0.5,1,1),calc.dist)$par
  par.est<-c(sig.est[1]^2,sig.est[2],sig.est[3]^2,sig.est[4]^2) #convert to sigsq0, sigma01, sigsq1,sigsqe
  Sigmat0<-par.est[1]*ones%*%t(ones)+par.est[2]*(ones%*%t(times)+times%*%t(ones))+par.est[3]*times%*%t(times)+sig.est[4]*diag(p)
  Sigmat0sq<-Sigmat0%*%Sigmat0
  #Calculate parameters for test stat & dist 
  B<-list(ones%*%t(ones),ones%*%t(times)+times%*%t(ones),times%*%t(times),diag(p)) #B partial derivatives w/o Sigmat0
  q=length(B)
  V0<-V1<-V2<-matrix(0,ncol=q,nrow=q)
  for(i in 1:q){
    for(j in 1:q){
      V0[i,j]<-tr(B[[i]]%*%B[[j]])
      V1[i,j]<-tr(B[[i]]%*%Sigmat0%*%B[[j]]%*%Sigmat0)
      V2[i,j]<-tr(B[[i]]%*%Sigmat0sq%*%B[[j]]%*%Sigmat0sq)
    }
  }
  omega0<-solve(V0)
  e1<-diag(epsilon.mat%*%B[[1]]%*%t(epsilon.mat))
  e2<-diag(epsilon.mat%*%B[[2]]%*%t(epsilon.mat))
  e3<-diag(epsilon.mat%*%B[[3]]%*%t(epsilon.mat))
  e4<-diag(epsilon.mat%*%B[[4]]%*%t(epsilon.mat))
  e<-cbind(e1,e2,e3,e4) #each row is a subj
    
  #Calculate Test stat
  sumT2<-tr(epsilon.mat%*%Sigmat0%*%t(epsilon.mat)) #2nd sum in Tn
  sumT1<-sum(sapply(c(1:(n-1)),function(i) sum((epsilon.mat[i,]%*%matrix(epsilon.mat[(i+1):n,],nrow=p,byrow=T))^2 )))
  sumJ2<-tr(e%*%omega0%*%t(e)) #2nd sum in Jn3
  sumJ1<-sum(sapply(c(1:(n-1)),function(i) sum(e[i,]%*%omega0%*%matrix(e[(i+1):n,],nrow=ncol(e),byrow=T))))
  C2n<-(n*(n-1))/2
  Tn1<-sumT1/C2n
  Tn2<-2*sumT2/n
  Tn3<-tr(Sigmat0sq)
  Tn<-Tn1-Tn2+Tn3 #Unbiased estimator
  Jn3<-(sumJ1/C2n-sumJ2/n)/n #Jn should be negative
  Lambdan<- Tn-Jn3 #Adjusted stat
    
  #Testing Adjusted stat (asymptotic normal)
  xi1sq<-(tr(Sigmat0sq)^2+
            tr(Sigmat0sq%*%Sigmat0sq)+
            2*tr(omega0%*%V1%*%omega0%*%V1)-
            4*tr(omega0%*%V2))
  varLambdan<-2*xi1sq/C2n #Var of Lambdan
  z.stat<-Lambdan/sqrt(varLambdan) #test stat
  p.z<-1-pnorm(z.stat)

  ### Fixed p (use weighted chi-sq)
  sumlambda=tr(Sigmat0)^2+tr(Sigmat0sq)-2*tr(omega0%*%V1)
  sumlambdasq=2*xi1sq
  g1=sumlambdasq/sumlambda
  g2=(sumlambda)^2/sumlambdasq
  g3=sumlambda
  
  chi.stat<-n*Lambdan  
  chi.samp<-c(g1*rchisq(10000,df=g2)-g3) #sample from weighted chi-sq
  p.chi<-mean(sapply(chi.samp,function(x) x>=chi.stat))
  list(C.alt=Sigmat1,C.null=Sigmat0,Lambdan=Lambdan,Tn=Tn,Jn3=Jn3,z.stat=z.stat,p.z=p.z,chi.stat=chi.stat,p.chi=p.chi,chi.samp=chi.samp)
}
