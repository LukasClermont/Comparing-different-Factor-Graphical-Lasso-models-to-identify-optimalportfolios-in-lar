#Estimate Omega using the Sherman-Morrison-Woodbury
get.Sherman.Morrison.Woodbury <- function(Omega_E, Omega_F, B_hat){
  Omega = Omega_E - Omega_E %*% B_hat %*% solve(Omega_F + t(B_hat) %*% Omega_E %*% B_hat) %*% t(B_hat) %*% Omega_E
  return(Omega)
}
