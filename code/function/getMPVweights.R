get.MVP.weights <- function(precisionmatrix){
  jota <- t(rep(1, nrow(precisionmatrix)))
  rel.opt.port.wei <- 1/(jota %*% precisionmatrix %*% t(jota))[[1]] * precisionmatrix %*% t(jota)
  return(rel.opt.port.wei)
}
