get.factormodel <- function(R, model, K = 1, factor = NULL){
  R = as.matrix(R) #R is a pxT matrix
  T = ncol(R) #Number of time periods
  p = nrow(R) #Number of assets
  eig.val = NULL
  
  #fit model
  if(model == "PCA"){
    #PCA
    # get eigen decomposition
    S = t(R) %*% R
    eig.decomp <- eigen(S, symmetric=TRUE)
    eig.val = eig.decomp$values[1:K]
    
    # Stock and Watson (2002) show that
    F_hat = sqrt(T) * t(eig.decomp$vector[,1:K]) #Choose number of factors
    
    B_hat =  (1/ T) * R %*% t(F_hat) #factor loading
    R_hat = B_hat %*% F_hat
  
  } else if(model == "FF" && !is.na(factor)){
    #Factor model
    F_hat = as.matrix(factor)
    
    B_hat =  R %*% t(F_hat) %*% solve(F_hat %*% t(F_hat))
    R_hat = B_hat %*% F_hat 
    
  } else {
    stop(paste0("Model name not found: ", model))
  }

  E_hat = R - R_hat #spare idiosyncratic Error term
  
  # compute covariance
  Sigma_F = get.cov(F_hat)
  Omega_F = get.inv(Sigma_F)
  Sigma_E = get.cov(E_hat)
  Omega_E = get.inv(diag(diag(Sigma_E)))
  
  Sigma_E_diag = diag(diag(Sigma_E))
  Sigma = B_hat %*% Sigma_F %*% t(B_hat) + Sigma_E_diag
  Omega = get.inv(Sigma)#using solve function of cholevsky inverse
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  
  return(list(omega = omega,
              Omega = Omega,
              factors = F_hat, 
              Sigma_F = Sigma_F,
              Omega_F = Omega_F,
              K = K, 
              loadings = B_hat, 
              error = E_hat, 
              Sigma_E = Sigma_E,
              Omega_E = Omega_E,
              eigenvalues = eig.val, 
              estimate = F_hat))
}
