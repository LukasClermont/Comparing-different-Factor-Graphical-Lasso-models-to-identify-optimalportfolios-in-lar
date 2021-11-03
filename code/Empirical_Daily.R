rm(list = ls())

folder <- "./data/temp/final_daily/"
file.data <- paste0(folder, "Final_Data_Day.R")

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
  R <- t(data$ER)
  date <- data$date
  #R <- R[1:50,]
  dim(R)
  colnames(R) <- data$date
  
  F_FF3 <- t(data$FF3[c("Mkt-RF", "SMB", "HML")])
  F_ETF <- t(data$F_index)
  F_FF3ETF <- rbind(F_FF3, F_ETF)
}
#IS and OOS
{
  date <- data$date
  R <- t(data$ER)
  dim(R)
  colnames(R) <- data$date
  R <- R[,as.character(date)]
  
  F_FF3 <- t(data$FF3[c("Mkt-RF", "SMB", "HML")])
  F_ETF <- t(data$F_index)
  F_FF3ETF <- rbind(F_FF3, F_ETF)
  result = NULL
  IS.start = 	1921   
  IS.end = IS.start + 340 #ncol(R)
  windowOOS = 340
  date[c(IS.start, IS.end)]
  Rt <- R[,IS.start:IS.end]
  Rt <- R[complete.cases(Rt),]#take assets with no missing values
  #---IS---
  #Equal
  results.Equal <- fit.model(Rt, model = "Equal", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #PCA
  results.PCA <- fit.model(Rt, K = 3, model = "PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #Farma/French
  results.FF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #Sector ETFs
  results.ETF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS)
  #Farma/French and Sector ETFs
  results.FFETF <- fit.model(Rt, model = "FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS)
  #Glasso
  results.Glasso <- fit.model(Rt, model = "Glasso", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
  #MB
  results.MB <- fit.model(Rt, model = "MB", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS)
  
  #FGL using PCA
  results.FGL.PCA <- fit.model(Rt, K = 3, lambda = NULL, model = "FGL.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #FGL using Farma/French
  results.FGL.FF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #FGL using EFTs
  results.FGL.ETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
  #FGL using Farma/French and EFTs
  results.FGL.FFETF <- fit.model(Rt, lambda =  NULL, model = "FGL.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 

  #Lambda Tuning Parameter
  results.FGL.PCA$result$lambda
  results.FGL.FF$result$lambda
  results.FGL.ETF$result$lambda
  results.FGL.FFETF$result$lambda
  
  #FMB using PCA
  results.FMB.PCA <- fit.model(Rt, K = 3, model = "FMB.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
  #FMB using Farma/French
  results.FMB.FF <- fit.model(Rt,  model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
  #FMB using EFTs
  results.FMB.ETF <- fit.model(Rt, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
  #FMB using Farma/French and EFTs
  results.FMB.FFETF <- fit.model(Rt,  model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 

  results_IS = cbind(results.Equal$results.IS,
                     results.PCA$results.IS, 
                     results.FF$results.IS, 
                     results.ETF$results.IS,
                     results.FFETF$results.IS,
                     results.Glasso$results.IS,
                     results.MB$results.IS,
                     results.FGL.PCA$results.IS, 
                     results.FGL.FF$results.IS,
                     results.FGL.ETF$results.IS,
                     results.FGL.FFETF$results.IS,
                     results.FMB.PCA$results.IS, 
                     results.FMB.FF$results.IS,
                     results.FMB.ETF$results.IS,
                     results.FMB.FFETF$results.IS)
  colnames(results_IS) = c("Equal","PCA", "FF", "ETF", "FFETF", "Glasso", "MB",
                           "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", 
                           "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF")
  results_OOS = cbind(results.Equal$results.OOS,
                     results.PCA$results.OOS, 
                     results.FF$results.OOS, 
                     results.ETF$results.OOS,
                     results.FFETF$results.OOS,
                     results.Glasso$results.IS,
                     results.MB$results.IS,
                     results.FGL.PCA$results.OOS, 
                     results.FGL.FF$results.OOS,
                     results.FGL.ETF$results.OOS,
                     results.FGL.FFETF$results.OOS,
                     results.FMB.PCA$results.OOS, 
                     results.FMB.FF$results.OOS,
                     results.FMB.ETF$results.OOS,
                     results.FMB.FFETF$results.OOS)
  colnames(results_OOS) = c("Equal","PCA", "FF", "ETF", "FFETF", "Glasso", "MB",
                           "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF",
                           "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF")
  results_OOS = format(results_OOS, scientific = TRUE)
}
#Rolling Window
{
  #(p,n_I, h)
  # parameter.list = list(
  #   c(62, 125, 300),
  #   c(187, 125, 300),
  #   c(250, 125, 300),
  #   c(500, 125, 300)
  # )
  parameter.list = list(
    c(75, 250, 300),
    c(375, 250, 300),
    c(500, 250, 300)
  )
  # parameter.list = list(
  #   c(62, 125, 300),
  #   c(184, 125, 300),
  #   c(250, 125, 300),
  #   c(500, 125, 300)
  # )
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
      
      Rt <- Rt[rowSums(is.na(Rt)) == 0,]
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
      
      #FMB using PCA
      results.FMB.PCA <- fit.model(Rt, K = 1, lambda =  NULL, model = "FMB.PCA", IS.start = IS.start, IS.end = IS.end, OOS.period = windowOOS) 
      #FMB using Farma/French
      results.FMB.FF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3, OOS.period = windowOOS) 
      #FMB using EFTs
      results.FMB.ETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_ETF, OOS.period = windowOOS) 
      #FMB using Farma/French and EFTs
      results.FMB.FFETF <- fit.model(Rt, lambda =  NULL, model = "FMB.FF", IS.start = IS.start, IS.end = IS.end, factor = F_FF3ETF, OOS.period = windowOOS) 
      
      results_i = cbind(results.Equal$results.OOS,
                        results.LW$results.OOS,
                        results.POET$results.OOS,
                        results.Glasso$results.OOS,
                        results.MB$results.OOS,
                        results.FGL.PCA$results.OOS, 
                        results.FGL.FF$results.OOS,
                        results.FGL.ETF$results.OOS,
                        results.FGL.FFETF$results.OOS,
                        results.FMB.PCA$results.OOS, 
                        results.FMB.FF$results.OOS,
                        results.FMB.ETF$results.OOS,
                        results.FMB.FFETF$results.OOS)
      colnames(results_i) = c("Equal", "LW", "POET", "Glasso", "MB",
                              "FGL.PCA", "FGL.FF", "FGL.ETF",  "FGL.FFETF", 
                              "FMB.PCA", "FMB.FF", "FMB.ETF",  "FMB.FFETF")
      results_i = as.data.frame(results_i)
      results_i$measure = rownames(results_i)
      results_i$time = paste("i",i, "IS",date[IS.start], "-" , date[IS.end], "OOS", date[IS.end + windowOOS+1]) 
      results_i$h = windowOOS
      results_i$windowIS = windowIS
      results_i$p = p
      result <- rbind(result, 
                      results_i)
      print(paste("h",h, "p", p, i, "/", windowmax))
      i = i + windowOOS
    }
  }
  
  result <- unique(result)
  
  file.monthly.results <- paste0("./results/Empirical/daily/results_daily_n250.R")
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
      FMB.PCA = mean(FMB.PCA),
      FMB.FF = mean(FMB.FF),
      FMB.ETF = mean(FMB.ETF),
      FMB.FFETF = mean(FMB.FFETF))
  View(t(result.sum))
  file.monthly.results <- paste0("./results/Empirical/daily/results_daily_n250_sum.R")
  save(result.sum, file = file.monthly.results)
}
