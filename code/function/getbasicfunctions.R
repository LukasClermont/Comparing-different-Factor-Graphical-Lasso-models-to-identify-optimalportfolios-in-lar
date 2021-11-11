# CALCULATE BASIC FUNCTIONS
## Portfolio measure, covariance, inverse, l1 and l2 norm, MVP weights, Toeplitz matrix and Sherman-Morrison-Woodbury formula

# Compute returns
get.return <- function(w, r){
  # Input:
  # W -- vector of estimated optimal portfolio weights
  # r -- matrix of OOSs returns 
  #
  # Output:
  # nu_test -- return in OOS (test) horizon

  n = ncol(r) # n = T-m for OOS and T for IS
  
  nu_test = 1/n * sum(t(w) %*% r)
  
  return(nu_test)
}

# Compute variance
get.variance <- function(w, r){
  # Input:
  # W -- vector of estimated optimal portfolio weights
  # r -- matrix of OOSs returns 
  #
  # Output:
  # var -- var in OOS horizon
  nu_test = get.return(w,  r)
  n = ncol(r) # n = T-m for OOS and T for IS
  
  var = 1/(n-1) * sum((t(w) %*% r - nu_test )^2)
  
  return(var)
}

# Compute sharp ratio
get.sr <- function(w, r){
  # Input:
  # W -- vector of estimated optimal portfolio weights
  # r -- matrix of OOSs returns 
  #
  # Output:
  # sr -- sharp ratio in OOS (test) horizon
  
  sr = get.return(w, r) / sqrt(get.variance(w, r))
  
  return(sr)
}

# Compute returns, variance and sharp ratio
get.measures <- function(w, r){
  # Input:
  # W -- vector of estimated optimal portfolio weights
  # r -- matrix of OOSs returns 
  #
  # Output:
  # result -- dataframe of return, variance and sharp ration
  rtn <- get.return(w,r)
  var <- get.variance(w,r)
  sr <- get.sr(w,r)
  result <- t(data.frame(avg.return = rtn, avg.var = var, avg.sr = sr))
  return(result)
}

# Compute sample covariance
get.cov <- function(M){
  # Input:
  # M -- n x k matrix 
  #
  # Output:
  # S -- Sample covariance
  M <- as.matrix(M)
  k <- ncol(M) #number of variables
  n <- nrow(M) #number of subjects
  
  S = crossprod(t(M))/n # Sample covariance matrix
  
  return(S)
}

# Compute inverse of a symmetric martix
get.inv <- function(sigma){
  # Input:
  # sigma -- symmetric matrix
  #
  # Output:
  # sigsmpinv -- inverse of sigma
  x <- try(solve(sigma),silent = TRUE) #try to calculate the inverse using the solve function
  if(class(x)[1]=='try-error'){
    sigsmpinv <- chol2inv(chol(sigma)) #if solve function results in error, calculate the inverse by the cholesky decomposition
  }else{
    sigsmpinv <- solve(sigma)
  } 
  
  return(sigsmpinv)  
}

# Calculate the l1-norm
get.norm1 <- function(x) max(colSums(abs(x)))

# Calculate the l2-norm
get.norm2 <- function(x) sqrt(max(eigen(x %*% t(x))$values))

# Calculate the MVP weights
get.MVP.weights <- function(precisionmatrix){
  # Input:
  # precisionmatrix -- precision matrix
  #
  # Output:
  # rel.opt.port.wei -- optimal portfolio weights given a estimated precision matrix 
  jota <- t(rep(1, nrow(precisionmatrix))) #vecot of ones
  rel.opt.port.wei <- 1/(jota %*% precisionmatrix %*% t(jota))[[1]] * precisionmatrix %*% t(jota)
  return(rel.opt.port.wei)
}

#Estimate Omega for factor graphical model using the Sherman-Morrison-Woodbury
get.Sherman.Morrison.Woodbury <- function(Omega_E, Omega_F, B_hat){
  # Input:
  # Omega_F -- precision matrix of factors
  # Omega_E -- precision matrix of errors
  # B_hat -- estimated weighting matix of the factor model
  #
  # Output:
  # Omega -- estimator for the precision matrix
  Omega = Omega_E - Omega_E %*% B_hat %*% solve(Omega_F + t(B_hat) %*% Omega_E %*% B_hat) %*% t(B_hat) %*% Omega_E
  return(Omega)
}

# Calculate Toeplitz matrix 
get.Toeplitz <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}

# Calculate Toeplitz matrix 
get.Toeplitz.new <- function(d, rho) {
  B <- matrix(nrow = d, ncol = d)
  for(i in 1:d){
    for(j in 1:d){
      B[i,j] = rho^abs((i-j))
    }
  }
  return(B)
}



