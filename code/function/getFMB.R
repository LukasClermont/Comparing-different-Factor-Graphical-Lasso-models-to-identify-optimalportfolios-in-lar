# COMPUTE FACTOR GRAPHICAL MODEL WITH MEINSHAUSEN AND BÜHLMANN ESTIMATOR
## Note: In order to compute a statistical factor model set the variable model to "FMB.PCA" and for macroeconomic and fundamental factor model to "FMB.FF".
get.FMB <- function(R, model, K=1, factor =NA){
  # Input:
  # R -- matrix of returns
  # model -- can be observable ("FF") or unobserble ("PCA")
  # k -- number of factors
  # factor -- matrix of observable factors
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix
  
  R = as.matrix(R)
  
  #1. Fit factor model for PCA or observable factors
  if(model == "FMB.PCA"){
    result <- get.factormodel(R = R, model = "PCA", K = K)
  } else if (model == "FMB.FF" && !is.null(factor)){
    result <- get.factormodel(R = R, model = "FF", K = K, factor = factor)
  } else{
    stop(paste0("Model name not found: ", model))
  }
  
  B_hat <- result$loadings
  Sigma_E <- result$Sigma_E
  Omega_F = result$Omega_F
  E = result$error
  
  #2. Nodewise Regression
  est.glasso = get.MB(E,model)
  Omega_E = est.glasso$Omega
  #Make Omega_E symetric following prodecure by Fan 2018 
  for(i in 1:nrow(Omega_E)){
    for(j in 1: ncol(Omega_E)){
      if(abs(Omega_E[i,j]) > abs(Omega_E[j,i])){
        Omega_E[i,j] = Omega_E[j,i]
      } else if(abs(Omega_E[i,j]) < abs(Omega_E[j,i])){
        Omega_E[j,i] = Omega_E[i,j]
      }
    }
  }
  #make Omega_E positive definite as in Callot, Kock, and Medeiros (2017), and Hautsch, Kyj, and Oomen (2012)
  #spectral decomposition
  eigen_E = eigen(Omega_E)
  L = eigen_E$values
  V = eigen_E$vectors
  thres = 0
  L[L<thres] = thres
  Omega_E = V %*% diag(L) %*% t(V) #since matrix is symmetric
  #diag(Omega_E) = diag(W)
  sparsity_Omega_E = sum(Omega_E <= thres) / length(Omega_E)
  if(sparsity_Omega_E > 0.99){
    diag(Omega_E) = 1
  }
  #3. Estimate Omega using the Sherman-Morrison-Woodbury
  Omega = get.Sherman.Morrison.Woodbury(Omega_E, Omega_F, B_hat)
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  return(list(omega = omega, 
              Omega = Omega,
              Omega_E = Omega_E, 
              Omega_F = Omega_F,
              K = K
  ))
}
