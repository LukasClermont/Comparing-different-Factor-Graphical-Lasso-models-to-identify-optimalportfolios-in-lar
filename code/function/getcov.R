get.cov <- function(M){

  M <- as.matrix(M)
  k <- ncol(M) #number of variables
  n <- nrow(M) #number of subjects
  
  S = crossprod(t(M))/n # Sample covariance matrix
  
  return(S)
}

get.inv <- function(sigma){
  
  x <- try(solve(sigma),silent = TRUE)
  if(class(x)[1]=='try-error'){
    sigsmpinv <- chol2inv(chol(sigma))
  }else{
    sigsmpinv <- solve(sigma)
  } 
  
  return(sigsmpinv)  
}