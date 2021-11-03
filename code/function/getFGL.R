#Factor Graphica Lasso
get.FGL <- function(R, model, lambda =NULL , K=1, factor =NA){
  
  R = as.matrix(R)
  
  #1. Fit model for PCA or Farma/French
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
  
  #2. Graphical Lasso
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

