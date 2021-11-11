# FUNCTION TO COMPUTE MODELS
## Note: FF, FGL.FF, FGL.MB are factor models using observable factors as input. Statistical factors are computed in PCA, FGL.PCA and MB.PCA.
fit.model <- function(R, K=1, lambda = NULL, factor = NULL, model = NULL, IS.start=NULL, IS.end=NULL, OOS.period=NULL){
  # Input:
  # R -- matrix of returns
  # k -- number of factors
  # lambda --  regularization parameter for glasso
  # factor -- matrix of factors
  # model -- choice of model (PCA, FF, Glasso, LW, POET, MB, FGL.PCA, FGL.FF, FMB.PCA, FMB.PCA) where FF requires observable factors like Fama-French factors as an input
  # IS.start -- integer variable to identify the start of the IS period
  # IS.end -- integer variable to identify the end of the IS period
  # OOS.period -- length h of the OOS period/horizon
  #
  # Output:
  # omega -- vector of portfolio weights
  # Omega -- estimator of precision matrix
  # results.IS -- results of IS measures
  # results.OOS -- results of OOS measures
  
  #define IS and OOS return
  if(!is.null(IS.start) && !is.null(IS.end)){
    R.IS <- R[,IS.start:IS.end]
  }
  if(!is.null(OOS.period)){
    R.OOS <- R[,(IS.end+1):(IS.end+1+OOS.period)]
  }
  
  #Fit model
  if(model == "PCA"){
    result.IS <- get.factormodel(R = R.IS, model = model, K = K)
    } else if (model == "FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end]
      result.IS <- get.factormodel(R = R.IS, model = model, K = K, factor = factor.IS)
    } else if (model == "Glasso"){
      result.IS <- get.GlassoEBIC(R = R.IS, lambda = lambda, model = model)
    } else if (model == "LW"){
      result.IS <- get.LW(R = R.IS)
    }else if (model == "POET"){
      result.IS <- get.POET(R = R.IS)
    }else if (model == "MB"){
      result.IS <- get.MB(R = R.IS, model = model)
    } else if (model == "FGL.PCA"){
      result.IS <- get.FGL(R = R.IS, model = model, lambda = lambda, K = K)
    }  else if (model == "FGL.FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end] #Select IS period of factor observations
      result.IS <- get.FGL(R = R.IS, model = model, lambda = lambda, factor = factor.IS)
    } else if (model == "FMB.PCA"){
      result.IS <- get.FMB(R = R.IS, model = model, K = K)
    }  else if (model == "FMB.FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end]  #Select IS period of factor observations
      result.IS <- get.FMB(R = R.IS, model = model, factor = factor.IS)
    } else if (model == "Equal"){
      result.IS <- NULL
      result.IS$omega <- rep(1/nrow(R.IS), nrow(R.IS)) #Set weights for each asset to 1/p
      result.IS$Omega <- get.cov(R.IS)
  }else{
      stop(paste0("Model name not found: ", model))
  }
  
  #Compute IS and OOS measures
  if(!is.null(IS.start) && !is.null(IS.end)){
    results.IS <- get.measures(result.IS$omega, R.IS)
  }
  if(!is.null(OOS.period)){
    results.OOS <- get.measures(result.IS$omega, R.OOS)
  }
  
  return(list(model = model, 
              omega = result.IS$omega, 
              Omega = result.IS$Omega, 
              results.IS = results.IS, 
              results.OOS = results.OOS
              )) 
}
