# COMPUTE FACTOR GRAPHICAL LASSO MODEL
## Note: In order to compute statistical factore models set model to "FGL.PCA" and for macroeconomic and fundamental factor models set model to "FGL.FF".
get.FGL <- function(R, model, lambda =NULL , K=1, factor =NA){
  # Input:
  # R -- matrix of returns
  # model -- can be observable ("FF") or unobserble ("PCA")
  # lambda -- penalty term, if "NULL" lambda is calculated using the eBIC
  # k -- number of factors
  # factor -- matrix of observable factors
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix
  # lambda -- if not predefined it is calculated by eBIC
  
  R = as.matrix(R)
  
  #1. Fit factor model for PCA or observable factors
  if(model == "FGL.PCA"){
    choice = FALSE
    result <- get.factormodel(R = R, model = "PCA", K = K)
  } else if (model == "FGL.FF" && !is.null(factor)){
    choice = FALSE
    result <- get.factormodel(R = R, model = "FF", factor = factor)
  } else{
    stop(paste0("Model name not found: ", model))
  }
  
  B_hat <- result$loadings
  Sigma_E <- result$Sigma_E
  Omega_F = result$Omega_F
  
  #2. Compute Graphical Lasso using the error terms
  est.glasso = get.GlassoEBIC(R = result$error,lambda = lambda, model =  model, choice = choice)
  Omega_E = est.glasso$Omega
  sparsity_Omega_E = sum(Omega_E == 0) / length(Omega_E)
  
  
  #3. Estimate Omega using the Sherman-Morrison-Woodbury
  Omega = get.Sherman.Morrison.Woodbury(Omega_E, Omega_F, B_hat)
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)

  return(list(omega = omega, 
              Omega = Omega,
              Omega_E = Omega_E, 
              Omega_F = Omega_F,
              K = K,
              lambda = est.glasso$lambda
              ))
}

