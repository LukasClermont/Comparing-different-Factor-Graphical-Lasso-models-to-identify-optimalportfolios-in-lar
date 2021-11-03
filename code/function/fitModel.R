fit.model <- function(R, K=1, lambda = NULL, factor = NULL, model = NULL, IS.start=NULL, IS.end=NULL, OOS.period=NULL){
  R = as.matrix(R)
  R.IS = NULL
  R.OOS = NULL
  results.IS = NULL 
  results.OOS = NULL
  
  #define IS and OOS return
  if(!is.null(IS.start) && !is.null(IS.end)){
    R.IS <- R[,IS.start:IS.end]
  }
  if(!is.null(OOS.period)){
    R.OOS <- R[,(IS.end+1):(IS.end+1+OOS.period)]
  }
  
  #Fit model for PCA or Farma/French
  if(model == "PCA"){
    result.IS <- get.factormodel(R = R.IS, model = model, K = K)
    } else if (model == "FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end]
      result.IS <- get.factormodel(R = R.IS, model = model, K = K, factor = factor.IS)
    } else if (model == "Glasso"){
      result.IS <- get.GlassoEBIC(R = R.IS, lambda = lambda, model = model)
    } else if (model == "LW"){
      result.IS <- get.LW(R = R.IS)
    } else if (model == "DCC"){
      result.IS <- get.DCC(R = R.IS)
    }else if (model == "POET"){
      result.IS <- get.POET(R = R.IS)
    }else if (model == "MB"){
      result.IS <- get.MB(R = R.IS, model = model)
    } else if (model == "FGL.PCA"){
      result.IS <- get.FGL(R = R.IS, model = model, lambda = lambda, K = K)
    }  else if (model == "FGL.FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end]
      result.IS <- get.FGL(R = R.IS, model = model, lambda = lambda, factor = factor.IS)
    } else if (model == "FMB.PCA"){
      result.IS <- get.FMB(R = R.IS, model = model, K = K)
    }  else if (model == "FMB.FF" && !is.null(factor)){
      factor.IS <- factor[,IS.start:IS.end]
      result.IS <- get.FMB(R = R.IS, model = model, factor = factor.IS)
    } else if (model == "Equal"){
      result.IS <- NULL
      result.IS$omega <- rep(1/nrow(R.IS), nrow(R.IS))
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
              results.OOS = results.OOS, 
              result = result.IS
              )) 
}
