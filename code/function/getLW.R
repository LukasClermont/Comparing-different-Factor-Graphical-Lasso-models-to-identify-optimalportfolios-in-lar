# LEDOIT-WOLF ESTIMATOR

#ledoit wolf version of the squared frobenius norm
lwfrob <- function(x){sum(diag(tcrossprod(x)))/ncol(x)}

# Estimate Ledoit-Wolf covariance matrix
get.LW <- function(R){
  # Input:
  # R - matrix of returns
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix
  
  Y = t(R)
  n <- nrow(Y)
  p <- ncol(Y)
  
  # sample cov mat
  S <- get.cov(R)
  
  m <- sum(diag(tcrossprod(S,diag(p))))/p

  d2 <- lwfrob(S-m*diag(p))

  b2bar <- sum(apply(Y,1,function(x,S){lwfrob(x%*%t(x)-S)},S=S))/n^2
  b2 <- min(d2,b2bar)

  a2 <- d2-b2
  
  # Computing the covariance matrix
  Sigma <- (b2/d2)*m*diag(p) + (a2/d2)*S
  # Computing the inverse
  Omega <- chol2inv(chol(Sigma))
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  return(list(
    omega = omega, 
    Omega = Omega
  ))
}
