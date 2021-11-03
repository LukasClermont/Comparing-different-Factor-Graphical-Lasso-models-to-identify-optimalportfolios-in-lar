get.return <- function(w, r){
  n = ncol(r) # n = T-m for OOS and T for IS
  
  nu_test = 1/n * sum(t(w) %*% r)
  
  return(nu_test)
}

get.variance <- function(w, r){
  nu_test = get.return(w,  r)
  n = ncol(r) # n = T-m for OOS and T for IS
  
  var = 1/(n-1) * sum((t(w) %*% r - nu_test )^2)
  
  return(var)
}

get.sr <- function(w, r){
  
  sr = get.return(w, r) / sqrt(get.variance(w, r))
  
  return(sr)
}


get.measures <- function(w, r){
  rtn <- get.return(w,r)
  var <- get.variance(w,r)
  sr <- get.sr(w,r)
  result <- t(data.frame(avg.return = rtn, avg.var = var, avg.sr = sr))
  return(result)
}
