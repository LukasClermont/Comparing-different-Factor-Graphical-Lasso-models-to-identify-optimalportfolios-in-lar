rm(list = ls())

folder <- "./data/temp/final_monthly/"
file.data <- paste0(folder, "Final_Data_Month.R")

#Get functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}
#Load data 
{
  load(file.data)
  date <- substr(data$FF3$date,1,6)
  R <- t(data$ER)
  #R <- R[1:50,]
  dim(R)
  colnames(R) <- substr(data$date,1,6)
  R <- R[,date]
  
  F_FF3 <- t(data$FF3[c("Mkt-RF", "SMB", "HML")])
  F_ETF <- t(data$F_index)
  F_FF3ETF <- rbind(F_FF3, F_ETF)
  F_Macro <- t(data$Macro[-1])
}
#One Period: IS and OOS
{
  result = NULL
  IS.start = 1 
  i = 1
  IS.end = IS.start + 120 #ncol(R)
  windowOOS = 1
  #---IS---
  #Equal
  results.Equal <- fit.model(R, model = "Equal", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #PCA
  results.PCA <- fit.model(R, K = 3, model = "PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #Farma/French
  results.FF <- fit.model(R, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #Sector ETFs
  results.ETF <- fit.model(R, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS)
  #Farma/French and Sector ETFs
  results.FFETF <- fit.model(R, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS)
  #Macroeconomic Factors
  results.Macro <- fit.model(R, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS)
  #Glasso
  results.Glasso <- fit.model(R, model = "Glasso", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
  #MB
  results.MB <- fit.model(R, model = "MB", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
  
  #FGL using PCA
  results.FGL.PCA <- fit.model(R, K = 3, lambda = NULL, model = "FGL.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #FGL using Farma/French
  results.FGL.FF <- fit.model(R, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #FGL using EFTs
  results.FGL.ETF <- fit.model(R, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
  #FGL using Farma/French and EFTs
  results.FGL.FFETF <- fit.model(R, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
  #FGL using Macro
  results.FGL.Macro <- fit.model(R, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
  
  #FMB using PCA
  results.FMB.PCA <- fit.model(R, K = 3, lambda =  NULL, model = "FMB.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #FMB using Farma/French
  results.FMB.FF <- fit.model(R, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #FMB using EFTs
  results.FMB.ETF <- fit.model(R, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
  #FMB using Farma/French and EFTs
  results.FMB.FFETF <- fit.model(R, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
  #FMB using Macroeconomic Factors
  results.FMB.Macro <- fit.model(R, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
  
  results_IS = cbind(results.Equal$results.IS,
                     results.PCA$results.IS, 
                     results.FF$results.IS, 
                     results.ETF$results.IS,
                     results.FFETF$results.IS,
                     results.Glasso$results.IS,
                     results.MB$results.IS,
                     results.Macro$results.IS,
                     results.FGL.PCA$results.IS, 
                     results.FGL.FF$results.IS,
                     results.FGL.ETF$results.IS,
                     results.FGL.FFETF$results.IS,
                     results.FGL.Macro$results.IS,
                     results.FMB.PCA$results.IS, 
                     results.FMB.FF$results.IS,
                     results.FMB.ETF$results.IS,
                     results.FMB.FFETF$results.IS,
                     results.FMB.Macro$results.IS)
  colnames(results_IS) = c("Equal","PCA", "FF", "ETF", "FFETF", "Macro", "Glasso", "MB",
                           "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", "FGL.Macro",
                           "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF", "FMB.Macro")
  results_OOS = cbind(results.Equal$results.OOS,
                     results.PCA$results.OOS, 
                     results.FF$results.OOS, 
                     results.ETF$results.OOS,
                     results.FFETF$results.OOS,
                     results.Macro$results.OOS,
                     results.Glasso$results.IS,
                     results.MB$results.IS,
                     results.FGL.PCA$results.OOS, 
                     results.FGL.FF$results.OOS,
                     results.FGL.ETF$results.OOS,
                     results.FGL.FFETF$results.OOS,
                     results.FGL.Macro$results.OOS,
                     results.FMB.PCA$results.OOS, 
                     results.FMB.FF$results.OOS,
                     results.FMB.ETF$results.OOS,
                     results.FMB.FFETF$results.OOS,
                     results.FMB.Macro$results.OOS)
  colnames(results_OOS) = c("Equal","PCA", "FF", "ETF", "FFETF", "Macro", "Glasso", "MB",
                           "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", "FGL.Macro",
                           "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF", "FMB.Macro")
}
#Rolling Window
{
  #(p,n_I, h)
    parameter.list = list(
      c(15, 30, 12),
      c(45, 30, 12),
      c(60, 30, 12),
      c(120, 30, 12),
      c(30, 60, 12),
      c(90, 60, 12),
      c(120, 60, 12),
      c(240, 60, 12)
    )
    result = NULL
    result.all = NULL
    
    for(parameter in parameter.list){
      p = parameter[1]
      windowIS = parameter[2]
      h = parameter[3]
      
      print(paste("IS",windowIS, "p", p,"h", h))
      set.seed(123)
      p.select = sample(x = 1:nrow(R),size=p, replace = FALSE)
      Rt = R[p.select,]
      windowOOS = h
      windowmax = ncol(Rt)-(windowIS+windowOOS+1)
      i = 1
      while(i  < windowmax){
        IS.start = i
        IS.end = i + windowIS
        
        #Equal
        results.Equal <- fit.model(Rt, model = "Equal", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
        #Glasso
        results.Glasso <- fit.model(Rt, model = "Glasso", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
        #MB
        results.MB <- fit.model(Rt, model = "MB", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
        #LW
        results.LW <- fit.model(Rt, model = "LW", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
        #DCC
        results.POET <- fit.model(Rt, model = "POET", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
        
        
        #FGL using PCA
        results.FGL.PCA <- fit.model(Rt, K = 1, lambda =  NULL, model = "FGL.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
        #FGL using Farma/French
        results.FGL.FF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
        #FGL using EFTs
        results.FGL.ETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
        #FGL using Farma/French and EFTs
        results.FGL.FFETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
        #FGL using Macro
        results.FGL.Macro <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
        
        
        #FMB using PCA
        results.FMB.PCA <- fit.model(Rt, K = 1, lambda =  NULL, model = "FMB.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
        #FMB using Farma/French
        results.FMB.FF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
        #FMB using EFTs
        results.FMB.ETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
        #FMB using Farma/French and EFTs
        results.FMB.FFETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
        #FMB using Macroeconomic Factors
        results.FMB.Macro <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
        
        results_i = cbind(results.Equal$results.OOS,
                          results.LW$results.OOS,
                          results.POET$results.OOS,
                          results.Glasso$results.OOS,
                          results.MB$results.OOS,
                          results.FGL.PCA$results.OOS, 
                          results.FGL.FF$results.OOS,
                          results.FGL.ETF$results.OOS,
                          results.FGL.FFETF$results.OOS,
                          results.FGL.Macro$results.OOS,
                          results.FMB.PCA$results.OOS, 
                          results.FMB.FF$results.OOS,
                          results.FMB.ETF$results.OOS,
                          results.FMB.FFETF$results.OOS,
                          results.FMB.Macro$results.OOS)
        colnames(results_i) = c("Equal", "LW", "POET", "Glasso", "MB",
                                "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", "FGL.Macro",
                                "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF", "FMB.Macro")
        results_i = as.data.frame(results_i)
        results_i$measure = rownames(results_i)
        results_i$time = paste("i",i, "IS",date[IS.start], "-" , date[IS.end], "OOS", date[IS.end + windowOOS+1]) 
        results_i$h = windowOOS
        results_i$windowIS = windowIS
        results_i$p = p
        results_i$yearoos = str_sub(date[IS.end + windowOOS+1],1,4)
        result <- rbind(result, 
                        results_i)
        print(paste("h",h, "p", p, i, "/", windowmax))
        i = i + windowOOS
      }
    }
    
  result <- unique(result)
  
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_n3060.R")
  save(result, file = file.monthly.results)
  load(file = file.monthly.results)
  result.sum = result%>% 
    group_by(measure, h, p, windowIS)%>%
    summarise(
      Equal = mean(Equal),
      LW = mean(LW),
      POET = mean(POET),
      Glasso = mean(Glasso),
      MB = mean(MB),
      FGL.PCA = mean(FGL.PCA),
      FGL.FF = mean(FGL.FF),
      FGL.ETF = mean(FGL.ETF),
      FGL.FFETF = mean(FGL.FFETF),
      FGL.Macro = mean(FGL.Macro),
      FMB.PCA = mean(FMB.PCA),
      FMB.FF = mean(FMB.FF),
      FMB.ETF = mean(FMB.ETF),
      FMB.FFETF = mean(FMB.FFETF),
      FMB.Macro = mean(FMB.Macro))
  View(t(result.sum))
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_lowhigh_n3060_sum.R")
  save(result.sum, file = file.monthly.results)
}
#Comparing FGL with Factor models
{
  #(p,n_I, h)
  parameter.list = list(
    c(30, 60, 12),
    c(60, 60, 12),
    c(90, 60, 12),
    c(120, 60, 12),
    c(180, 60, 12),
    c(210, 60, 12),
    c(240, 60, 12)
  )
  result = NULL
  result.all = NULL
  
  for(parameter in parameter.list){
    p = parameter[1]
    windowIS = parameter[2]
    h = parameter[3]
    
    print(paste("IS",windowIS, "p", p,"h", h))
    set.seed(123)
    p.select = sample(x = 1:nrow(R),size=p, replace = FALSE)
    Rt = R[p.select,]
    windowOOS = h
    windowmax = ncol(Rt)-(windowIS+windowOOS+1)
    i = 1
    while(i  < windowmax){
      IS.start = i
      IS.end = i + windowIS
      
      #PCA
      results.PCA <- fit.model(Rt, K = 3, model = "PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
      #Farma/French
      results.FF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
      #Sector ETFs
      results.ETF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS)
      #Farma/French and Sector ETFs
      results.FFETF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS)
      #Macroeconomic Factors
      results.Macro <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS)
      
      #FGL using PCA
      results.FGL.PCA <- fit.model(Rt, K = 1, lambda =  NULL, model = "FGL.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
      #FGL using Farma/French
      results.FGL.FF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
      #FGL using EFTs
      results.FGL.ETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
      #FGL using Farma/French and EFTs
      results.FGL.FFETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
      #FGL using Macro
      results.FGL.Macro <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
      
      
      #FMB using PCA
      results.FMB.PCA <- fit.model(Rt, K = 1, lambda =  NULL, model = "FMB.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
      #FMB using Farma/French
      results.FMB.FF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
      #FMB using EFTs
      results.FMB.ETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
      #FMB using Farma/French and EFTs
      results.FMB.FFETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
      #FMB using Macroeconomic Factors
      results.FMB.Macro <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_Macro, OOS.period = windowOOS) 
      
      results_i = cbind(results.PCA$results.OOS,
                        results.FF$results.OOS,
                        results.ETF$results.OOS,
                        results.FFETF$results.OOS,
                        results.Macro$results.OOS,
                        results.FGL.PCA$results.OOS, 
                        results.FGL.FF$results.OOS,
                        results.FGL.ETF$results.OOS,
                        results.FGL.FFETF$results.OOS,
                        results.FGL.Macro$results.OOS,
                        results.FMB.PCA$results.OOS, 
                        results.FMB.FF$results.OOS,
                        results.FMB.ETF$results.OOS,
                        results.FMB.FFETF$results.OOS,
                        results.FMB.Macro$results.OOS)
      colnames(results_i) = c("PCA", "FF", "ETF", "FFETF", "Macro",
                              "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", "FGL.Macro",
                              "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF", "FMB.Macro")
      results_i = as.data.frame(results_i)
      results_i$measure = rownames(results_i)
      results_i$time = paste("i",i, "IS",date[IS.start], "-" , date[IS.end], "OOS", date[IS.end + windowOOS+1]) 
      results_i$h = windowOOS
      results_i$n = windowIS
      results_i$p = p
      results_i$pn = results_i$p / results_i$n 
      results_i$yearoos = str_sub(date[IS.end + windowOOS+1],1,4)
      result <- rbind(result, 
                      results_i)
      print(paste("h",h, "p", p, i, "/", windowmax))
      i = i + windowOOS
    }
  }
  result <- unique(result)
  
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_Factor_FGL.R")
  save(result, file = file.monthly.results)
  load(file = file.monthly.results)
  result.sum = result%>% 
    filter(measure == "avg.var")
  result.sum = result.sum %>%
    group_by(h,n,p, pn)%>%
    summarise(
      PCA = mean(PCA),
      FGL.PCA = mean(FGL.PCA),
      FMB.PCA = mean(FMB.PCA),
      FF = mean(FF),
      FGL.FF = mean(FGL.FF),
      FMB.FF = mean(FMB.FF),
      ETF = mean(ETF),
      FGL.ETF = mean(FGL.ETF),
      FMB.ETF = mean(FMB.ETF),
      FFETF = mean(FFETF),
      FGL.FFETF = mean(FGL.FFETF),
      FMB.FFETF = mean(FMB.FFETF),
      Macro = mean(Macro),
      FGL.Macro = mean(FGL.Macro),
      FMB.Macro = mean(FMB.Macro))
  View(t(result.sum))
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_Factor_FGL_sum.R")
  save(result.sum, file = file.monthly.results)
}
