# CALCULATE GLASSO USING EBIC
get.GlassoEBIC <- function(R, model , lambda =NULL , lambda.min.ratio = 0.001,   nlambda = 20, choice = FALSE){
  # Input:
  # R -- matrix of returns
  # model -- can be observable ("FF") or observable ("PCA")
  # lambda -- penalty term, if "NULL" lambda is calculated using the eBIC
  # lambda.min.ratio -- minimum lambda for grid search to calculate EBIC
  # nlambda -- number of lambda candidates for grid search to calculate EBIC
  # choice -- False to calculate Glasso
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix
  # lambda -- if not predefined it is calculated by eBIC
  
  S = get.cov(R)
  #Grid for lambda (code taken from huge package)
  lambda.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
  lambda.min = lambda.min.ratio*lambda.max
  glasso.grid.lambda = exp(seq(log(lambda.min), log(lambda.max), length = nlambda))
  #Grid Search
  if(exists("i")) print(paste("i:", i, model, "Grid search"))
  if(is.null(lambda)){
    EBIC = NULL
    # grid.lambda = seq(0.001, 0.2, by = 0.002)
    for(lambda.i in glasso.grid.lambda){
      W = S + lambda.i * diag(x = 1, nrow = nrow(S), ncol = ncol(S) ) #Set starting value W
      est.glasso = glasso(s = S, rho = lambda.i, w.init = W, approx = choice)
      Omega = est.glasso$wi
      EBIC_i = get.EBIC(S, Omega, ncol(R), gamma = 1)#get.lambda.EBIC(Omega, W, nrow(R), ncol(R))
      EBIC_i = cbind(lambda.i, EBIC_i)
      EBIC = as.data.frame(rbind(EBIC, EBIC_i))
    }
    EBIC = EBIC %>% filter(EBIC_i > -999999999999)#avoid infinity
    if(nrow(EBIC)>0) EBIC = EBIC %>% filter(EBIC_i == min(EBIC$EBIC_i))
    lambda = EBIC$lambda.i
    
    if(nrow(EBIC)==0){
      lambda = 0.01
    }
  }
  print(paste("Choose lambda: ", lambda))
  #Calculate final Glasso
  W = S + lambda * diag(x = 1, nrow = nrow(S), ncol = ncol(S) ) #Set starting value W
  est.glasso = glasso(s = S, rho = lambda, w.init = W, approx = choice)
  
  Omega = est.glasso$wi
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  return(list(omega = omega, 
              Omega = Omega,
              lambda = lambda
  ))
}

logGaus <- function(S,K,n){
  tr = function(A) sum(diag(A))
  
  #avoid that det is shown as infinity in R
  L = chol(K) #det(K) = det(L) * det(L') -> log(det(K)) = log(det(L)*det(L')) = log(det(L))+ log(det(L'))
  
  return(n/2 * (log(det(L)) + log(det(t(L))) - tr(K %*% S))  )
  # return(n/2 * (log(det(K)) - tr(SK))  )
}

# Computes the EBIC:
get.EBIC <- function(S,K,n,gamma = 1,E,countDiagonal=FALSE){
  L <- logGaus(S, K, n)
  if (missing(E)){
    E <- sum(K[lower.tri(K,diag=countDiagonal)] != 0) #sum of all edges
  }
  p <- nrow(K)
  
  # return EBIC:
  -2 * L + E * log(n) + 4 * E * gamma * log(p)  
}
