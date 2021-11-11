# COMPUTE MONTE CARLO SIMULATION

# Simulate dataset
get.hugesim <- function (n, d, v, u, prob) 
{
  # Input:
  # n -- number of observations
  # d -- number of assets
  # v -- v controls the magnitude of partial correlationsparameter (default value 0.3)
  # u -- positive number added to the diagonal of the precision matrix to control the magnitude of partial correlations (default value 0.1)
  # prob -- popability that an enty of the matrix is zero
  #
  # Output:
  # Omega -- simulation of the precision matrix
  
  #Define a p x p adjacency matrix A which is used to represent the structure of the graph
  theta = matrix(0, d, d)
  tmp = matrix(runif(d^2,min = 0, max = 0.5), d, d)
  tmp = tmp + t(tmp) #to get symetric matrix
  theta[tmp < prob] = 1 #1 with prob equal to prob
  rm(tmp)
  
  diag(theta) = 0
  eigen.min = abs(min(eigen(theta * v)$values))
  diag.value = eigen.min + 0.1 + u
  omega =  theta * v + diag(diag.value, d,d)
  sigma = solve(omega)
  omega = solve(sigma)
  x = mvrnorm(n, rep(0, d), sigma)
  sigmahat = cov(x)
  sim = list(data = x, sigma = sigma, sigmahat = sigmahat, 
             omega = omega, theta = theta, 
             sparsity = sum(theta)/(d * (d - 1)))
  class(sim) = "sim"
  return(sim)
}

get.MonteCarlo <- function(delta, h, u, v, setseed, dim){
  # Input:
  # delta -- parameter for the size of p (p = 3* T^delta)
  # n -- number of observations
  # p -- number of assets
  # v -- v controls the magnitude of partial correlationsparameter (default value 0.3)
  # u -- positive number added to the diagonal of the precision matrix to control the magnitude of partial correlations (default value 0.1)
  # K -- number of common factors
  #
  # Output:
  # sim -- simulated dataset
  set.seed(setseed)
  T = round(2^h)
  p = 3*round(T^delta)
  q = 1/(p*T^0.8)
  K = round(2*(log(T))^0.5)
  #Simulate sparse matrix
  sim.mc <- get.hugesim(
    n = T,
    d = p,
    v = v,
    u = u,
    prob = q
  )
  epsilon = t(sim.mc$data)
  Omega_e <- sim.mc$omega
  
  #Create Toeplitz matrix
  rho = 0.2
  tplitz.cor <- get.Toeplitz.new(p, 0.2)
  
  #Cholesky decomposition 
  M = tplitz.cor
  M.chol <- chol(M)
  
  #Take first K rows
  B = t(M.chol[1:K,])
  
  # Simulate factors structure
  phi_f = 0.2
  sigma = 1
  f = matrix(nrow = T, ncol = K)
  Zeta_1 = rnorm(K, mean = 0, sd = sigma)
  f[1,] = Zeta_1 
  
  for(t in 2:T){
    Zeta_t = rnorm(K, mean = 0, sd = sigma)
    f[t,] = phi_f * f[t-1, ] + Zeta_t
  }
  f = t(f)
  
  # calculate returns 
  r = B %*% f + epsilon
  
  Omega_f = cov(t(f))^-1
  
  #Built Covariance of Simulation following Sherman Morrison Woodbury
  Omega = get.Sherman.Morrison.Woodbury(Omega_e, Omega_f, B)
  
return(list(return = r, B = B, f = f, epsilon = epsilon,Omega = Omega, Omega_f = Omega_f, Omega_e = Omega_e, T = T, p = p, K = K, q = q))
}
