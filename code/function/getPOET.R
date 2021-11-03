# construct the partial forward matrix reconstruction for POET with given k
sum_ev_back_k <- function(eval,evec,k){
  #init
  tmp <- matrix(0,length(eval),length(eval))
  for(j in (k+1):length(eval)){
    tmp <- tmp + eval[j]*(evec[,j]%*%t(evec[,j]))
  }
  return(tmp)
}

# construct the partial forward matrix reconstruction for POET with given k
sum_ev_forward_k <- function(eval,evec,k){
  #init
  tmp <- matrix(0,length(eval),length(eval))
  for(j in 1:k){
    tmp <- tmp + eval[j]*(evec[,j]%*%t(evec[,j]))
  }
  return(tmp)
}

# POET covariance matrix estimation
get.POET <- function(R,k=3){
  ssmp = get.cov(R)
  p = nrow(R)
  n = ncol(R)
  # Eigen value/vector decomposition
  ev <- eigen(ssmp)
  eval <- ev$values
  evec <- ev$vectors
  
  Omegahat <- sum_ev_back_k(eval,evec,k)
  
  # Construct 1st part of Sigma POET
  if(k==0)Spoet <- matrix(0,p,p)
  
  if(k!=0)Spoet <- sum_ev_forward_k(eval,evec,k)  
  
  
  # Looping over C until covariance matrix invertible: 
  pinv <- TRUE
  Clev <- 0
  while(pinv){
    # Construct threshold matrix
    Cthres <- Clev*(sqrt(log(p)/n)+sqrt(1/p))
    domega <- matrix(diag(Omegahat),ncol=1)
    momega <- Cthres * sqrt(domega%*%t(domega))
    diag(momega) <- 0
    
    # hard thresholding
    # Omegahat[abs(Omegahat)<momega] <- 0
    # soft thresholding
    Omegahat[abs(Omegahat)<momega] <- 0
    Omegahat[abs(Omegahat)>=momega] <- sign(abs(Omegahat[abs(Omegahat)>=momega]))*(abs(Omegahat[abs(Omegahat)>=momega])-momega[abs(Omegahat)>=momega])
    
    
    # Constructing the poet estimator 
    Stmp <- Spoet + Omegahat
    
    # checking if positive definite
    try.inv <- try(chol(Stmp),silent = TRUE)
    pinv <- class(try.inv)[1]=='try-error'
    if(pinv) Clev <- Clev + 0.1
    if(!pinv) {
      invSpoet <- try(chol2inv(try.inv))
      if(class(invSpoet)[1]=='try-error') Clev <- Clev + 0.1
    }
    
  }
  # Storing final POET estimator
  Sigma <- Stmp
  Omega <- invSpoet
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  return(list(
    omega = omega, 
    Omega = Omega
  ))
}
