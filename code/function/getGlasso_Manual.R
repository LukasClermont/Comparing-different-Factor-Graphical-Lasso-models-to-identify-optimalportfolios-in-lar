graphicalLasso <- function(S, rho = 0.1, maxIt = 1e2, tol = 1e-3){
  # Source: Friedman et al. (2007) 
  # Note: lasso Shooting function
  # Input:
  # S -- sample covariance matrix
  # rho --  regularization parameter
  # maxIt -- maximum number of iterations
  # tol -- convergence tolerance level
  # 
  # Output:
  # Wi -- inverse covariance matrix estimate
  # W -- regularized covariance matrix estimate, W = Theta^-1
  
  p = nrow(S)
  
  #Initialization
  W = S + rho * diag(1, nrow = p, ncol = p)   #diagonal of W remains unchanged
  W_old = W;
  Theta = matrix(nrow = p, ncol= p)
  B = matrix(nrow = p-1, ncol= p-1)
  i = 0
  gapW = 1
  #Graphical Lasso loop
  while(i < maxIt && gapW > tol){
    W_old = W
    i = i+1
    for(j in 1:(p-1)){
      jminus = setdiff(1:p,j)
      V = eigen(W[jminus,jminus])$vector
      D = eigen(W[jminus,jminus])$values
      d = diag(D)
      X = V %*% diag(sqrt(D)) %*% t(V) # W_11^(1/2)
      Y = V %*% diag(1./sqrt(D)) %*% t(V) %*% S[jminus,j]     #W_11^(-1/2) * s_12
      b1 = lassoShooting(X, Y, rho, maxIt, tol)
      W[jminus,j] = W[jminus,jminus] %*% b
      W[j,jminus] = t(W[jminus,j])
      B[,j] = b
      print(paste("Gap:", gapW,"Outer",i," - Inner:", round(100 * j/(p-1), 2),"%")) 
    }
    gapW = get.norm1(W-W_old)
  }
if(i == maxIt){
  print('Maximum number of iteration reached, glasso may not converge.');
}
  #Theta
  for(j in 1:(p-1)){
    b = B[,j]
    w_22 = as.matrix(W[j,j])
    w_12 = as.matrix(W[-j,j])
    theta_22 = 1/(w_22-t(w_12)%*% b)
    theta_12 = -b*theta_22[[1]]
    Theta[-j,j] = theta_12
    Theta[j,-j] = t(theta_12)
    Theta[j,j] = theta_22
  }

return(list(wi = Theta, w = W))
}



# Shooting algorithm for Lasso (unstandardized version)
lassoShooting <- function(X, Y, lambda = 0.1, maxIt = 1e2, tol = 1e-3){
  
  #Initialization
  n = nrow(X)
  p = ncol(X)

  if(p > n){
    b = rep(0, p) # From the null model, if p > n
  }else{
    b = Y
  }
  
  b_old = b;
  i = 0
  
  # Precompute X'X and X'Y
  XTX = t(X)%*%X
  XTY = t(X)%*%Y
  delta = 1
  #Shooting loop
  while(i < maxIt && delta > tol){
    i = i+1
    for(j in 1:p){
      jminus = setdiff(1:p,j)
      S0 = XTX[j,jminus]%*%b[jminus] - XTY[j]  # S0 = X(:,j)'*(X(:,jminus)*b(jminus)-Y)
        if(S0 > lambda){
          b[j] = (lambda-S0) / get.norm2(X[,j])^2
        }else if(S0 < -lambda){
          b[j] = -(lambda+S0) / get.norm2(X[,j])^2
        }else{
          b[j] = 0
        }
    }
    
    delta = get.norm1(b-b_old) # Norm change during successive iterations
    b_old = b
  }


if(i == maxIt){
  print('Maximum nuber of iteration reached, lasso may not converge')
}
return(b)
}
