get.DCC <- function(R){
  Y = t(R)
  uspec <- ugarchspec(mean.model=list(armaOrder=c(0, 0), include.mean=T),
                      variance.model=list(model="sGARCH", garchOrder=c(1,1), variance.targeting=F),
                      distribution.model="norm")
  
  spec <- dccspec(uspec=multispec(replicate(ncol(Y), uspec)),
                  dccOrder=c(1,1),
                  distribution="mvnorm")
  
  fit <- dccfit(data=Y, spec=spec, solver=c('hybrid', 'solnp'), fit.control=list(eval.se=F, scale=F))
  
  # Get last covariance
  Sigma <- rcov(fit)[ , , nrow(Y)]
  Omega <- get.inv(Sigma)
  
  
  # compute MVP weights
  omega = get.MVP.weights(Omega)
  
  return(list(
    omega = omega, 
    Omega = Omega
  ))
}



