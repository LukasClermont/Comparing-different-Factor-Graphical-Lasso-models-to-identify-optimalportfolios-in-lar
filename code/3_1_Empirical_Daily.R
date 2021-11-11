#EMPIRICAL EXPERMINET WITH DAILY DATA
rm(list = ls())
folder <- "./data/temp/"
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
  #load data
  load(file = "./data/temp/Final_Data_Day_1.R")
  load(file = "./data/temp/Final_Data_Day_2.R")
  data <- c(data1, data2)
  rm(data1, data2)
  R <- t(data$ER)
  date <- data$date
  dim(R)
  colnames(R) <- data$date
  
  F_FF3 <- t(data$FF3[c("Mkt-RF", "SMB", "HML")])
  F_ETF <- t(data$F_index)
  F_FF3ETF <- rbind(F_FF3, F_ETF)
}
#Rolling Window
{
  #First experiment with n_I = 125
  #(p,n_I, h)
  # parameter.list = list(
  #   c(62, 125, 300),
  #   c(187, 125, 300),
  #   c(250, 125, 300),
  #   c(500, 125, 300)
  # )
  #Second experiment with n_I = 250
  #(p,n_I, h)
  parameter.list = list(
    c(75, 250, 300),
    c(375, 250, 300),
    c(500, 250, 300)
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
    #Loop over every window
    while(i  < windowmax){
      IS.start = i
      IS.end = i + windowIS
      
      Rt <- Rt[rowSums(is.na(Rt)) == 0,]#Remove stocks with missing observations in the period
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
  
  #Save results
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
