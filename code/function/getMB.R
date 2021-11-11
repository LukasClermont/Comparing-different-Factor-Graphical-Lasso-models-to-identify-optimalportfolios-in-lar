# COMPUTE MEINSHAUSEN AND BÜHLMANN ESTIMATOR
get.MB <- function(R){
  # Input:
  # R -- matrix of returns
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix  
  # sp -- sparsity of the precision matrix
  # jbic -- information criterion (GIC)
  
  
    # initialization
    R = t(R)
    p <- ncol(R)
    n <- nrow(R)
    #R <- R- t((apply(R,2,mean))%*%matrix(1,1,n)) # R is de-meaned
    
    C <- matrix(0,p,p)
    diag(C) <- 1
    tau <- NULL
    
    # Loop over the assets
    for(j in 1:p){
      # Estimate the Lasso
      jlas <- glmnet(x=R[,-j],y=R[,j],family = 'gaussian',intercept = FALSE, maxit =1000000)
      # Get fit
      jfit <- predict(jlas, newx=R[,-j], type="response")    
      # residuals
      jres <- matrix(R[,j],n,length(jlas$lambda)) - jfit
      # std err
      jsig <- colSums(jres^2)/n
      # Computing information criterion (GIC)
      jbic  <- log(jsig) + jlas$df *log(p)/n * log(log(n)) # GIC
      # Index of selected model 
      jind  <- which.min(jbic)
      # Get the parameters
      jpar <- jlas$beta[,jind]
      # Computing tau squared
      jtau <- sum(jres[,jind]^2)/n + (1/2)*jlas$lambda[jind]*sum(abs(jpar)) # using (10)
      # Storing the parameters
      C[j,-j] <- -jpar
      tau <- c(tau,jtau)
    }
    
    # Construct T-squared inverse
    T2inv <- diag(1/tau)
    
    # Construct Theta-hat
    Theta <- T2inv %*% C
    
    # sparsity
    sp <- sum(Theta==0)/(p^2)
    
    
    # compute MVP weights
    omega = get.MVP.weights(Theta)
    
    return(list(omega = omega, 
                Omega = Theta,
                sp = sp, 
                jbic = jbic
    ))
}
