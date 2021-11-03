get.Toeplitz <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}


get.Toeplitz.new <- function(d, rho) {
  B <- matrix(nrow = d, ncol = d)
  for(i in 1:d){
    for(j in 1:d){
      B[i,j] = rho^abs((i-j))
    }
  }
  return(B)
}
