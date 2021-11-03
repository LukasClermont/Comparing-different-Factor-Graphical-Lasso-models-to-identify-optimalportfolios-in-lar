rm(list = ls())
library("Hmisc")
library(xtable)

#Get functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}
#Load data 
{
  folder <- "./data/temp/final_daily/"
  file.data <- paste0(folder, "Final_Data_Day.R")
  load(file.data)
}
#Plot Eigenvalues 
{
  R <- t(data$ER)
  R <- R[complete.cases(R),]#take assets with no missing values
  test <- eigen(get.cov(R[,4000:5000]))
  plot(test$values)
}
#Sparse inverse Matrix 
{
  test <- matrix(0, nrow = 10, ncol = 10)
  diag(test) = 1
  diag(test[-1,]) = 0.01
  diag(test[,-1]) = 0.01
  inv.test <- as.matrix(get.inv(test))
  inv.test <- formatC(inv.test, format = "e", digits = 2)
  bmatrix = function(x, digits=NULL, ...) {
    library(xtable)
    default_args = list(include.colnames=FALSE, only.contents=TRUE,
                        include.rownames=FALSE, hline.after=NULL, comment=FALSE,
                        print.results=FALSE)
    passed_args = list(...)
    calling_args = c(list(x=xtable(x, digits=digits)),
                     c(passed_args,
                       default_args[setdiff(names(default_args), names(passed_args))]))
    cat("\\begin{bmatrix}\n",
        do.call(print.xtable, calling_args),
        "\\end{bmatrix}\n")
  }
  bmatrix(inv.test)
}
#Plot number of GICS Sectors
{
  data.GICS <- as.data.frame(data$stockinfo)
  data.GICS <- data.GICS %>% 
    group_by(`GICS Sector`) %>% 
    summarise( Numberofassets = n())
 plot.GICS <- data.GICS %>% ggplot(aes(y= reorder(`GICS Sector`,Numberofassets), x = Numberofassets))+
   geom_bar(stat = "identity")+
   labs(title="",
        x ="# of assets", y = "")
 png(file="./results/plots/plot2_GICS.png",
     width=600, height=350)
 plot.GICS
 dev.off()
}
#Overview Stocks
{
  tx.overview <- data$stockinfo[c(1,2,4,5)]
  tx.overview$no. <- rownames(tx.overview)
  tx.overview <- tx.overview[c(5,1,2,3,4)]
  
  print(xtable(tx.overview[1:100,]), include.rownames=FALSE)
  print(xtable(tx.overview[101:200,]), include.rownames=FALSE)
  print(xtable(tx.overview[201:300,]), include.rownames=FALSE)
  print(xtable(tx.overview[301:400,]), include.rownames=FALSE)
  print(xtable(tx.overview[401:505,]), include.rownames=FALSE)
}
#Compare Factor and Factor Graphical Models - Monthly
{
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_Factor_FGL_sum.R")
  load(file = file.monthly.results)
  data.monthly <- result.sum %>% gather("model", "var", - pn, -h,-p,-n)
  data.monthly <- data.monthly %>% 
    mutate(Factor = case_when(grepl("PCA", model, fixed = TRUE) ~ "PCA",
                              grepl("FFETF", model, fixed = TRUE) ~ "FFETF",
                              grepl("FF", model, fixed = TRUE) ~ "FF",
                              grepl("ETF", model, fixed = TRUE) ~ "ETF",
                              grepl("Macro", model, fixed = TRUE) ~ "Macro"),
           Model = case_when(model %in% c("PCA", "FFETF", "FF", "ETF", "Macro") ~ "Factor",
                             grepl("FGL", model, fixed = TRUE) ~ "FGL",
                             grepl("MB", model, fixed = TRUE) ~ "FMB"))
  ggplot.Compare <- data.monthly %>% ggplot()+
    geom_line(aes(pn, var, color = Model))+
    geom_point(aes(pn, var, color = Model))+
    facet_grid(~Factor)+
    ylab("average Variance")+
    xlab("p / n")+
    theme_minimal()+ 
    scale_color_manual(values=c("#4d6941", "#4896a7", "#4a3341"))+ 
    theme(legend.position="bottom", legend.title = element_blank())
  
  png(file="./results/plots/plot4_CompareFactorGraphicalFactor_new.png",
      width=1000, height=500)
  ggplot.Compare
  dev.off()
}
#Results Monthly
{
  file.monthly.results <- paste0("./results/Empirical/monthly/results_monthly_lowhigh_n3060_sum.R")
  load(file.monthly.results)
  
  results.Monthly <- as.data.frame(result.sum)
  results.Monthly <- results.Monthly[-2]#remove horizon column as it is fix
  
  results.Monthly[-c(1:3)] <- formatC(as.matrix(results.Monthly[-c(1:3)]), format = "e", digits = 2)
  
  results.Monthly.var <- results.Monthly %>% filter(measure == "avg.var")
  results.Monthly.returnsr <- results.Monthly %>% filter(measure %in% c("avg.sr", "avg.return"))
  min.var <- apply(as.matrix(results.Monthly.var[-c(1:3)]), 1, min)
  max.ret.sr <- apply(as.matrix(results.Monthly.returnsr[-(1:3)]), 1, max)
  min.max <- c(max.ret.sr, min.var)
  
  results.Monthly <- results.Monthly %>% 
    mutate(pn = p/ windowIS,
           period = paste("n =",windowIS,", p =", p, "p/n", pn)) %>% 
    arrange(windowIS,pn, match(measure, c("avg.return", "avg.var", "avg.sr")))
  
  #rearange columns
  results.Monthly <- results.Monthly[c(20,1,4:18)]
  
  # Get rid of repeated periods
  for(i in nrow(results.Monthly):2) {
    if(identical(results.Monthly$measure[i-1], results.Monthly$measure[i])) {
      results.Monthly$measure[i] = ""
    }
    if(identical(results.Monthly$period[i-1], results.Monthly$period[i])) {
      results.Monthly$period[i] = ""
    }
  }
  results.Monthly <- t(results.Monthly)
  
  #Plot Table
  xftbl <- xtable(results.Monthly[,1:12], caption = "Monthly portfolio returns, risk, Sharpe Ratio (SR). Data set: December 1998 - June 2021 (270 obs). Models that perform best in the respective period are marked bold.")
  align(xftbl) <- "c|cccccccccccc"
  
  print.xtable(xftbl, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               floating = TRUE, 
               #floating.environment = "sidewaystable",
               sanitize.text.function = function(x) {
                 x <- latexTranslate(x)
                 x <- ifelse(x %in% min.max, paste0("\\textbf{", x, "}"), x)
                 return(x)
               })
  
  xftbl <- xtable(results.Monthly[,13:24], caption = "Monthly portfolio returns, risk, Sharpe Ratio (SR). Data set: December 1998 - June 2021 (270 obs). Models that perform best in the respective period are marked bold.")
  align(xftbl) <- "c|cccccccccccc"
  
  print.xtable(xftbl, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               floating = TRUE, 
               #floating.environment = "sidewaystable",
               sanitize.text.function = function(x) {
                 x <- latexTranslate(x)
                 x <- ifelse(x %in% min.max, paste0("\\textbf{", x, "}"), x)
                 return(x)
               })
  
}
#Results daily
{
  file.daily.results <- paste0("./results/Empirical/daily/results_daily_n125_sum.R")
  load(file.daily.results)
  
  results.Daily <- as.data.frame(result.sum)
  results.Daily <- results.Daily[-2]#remove horizon column as it is fix
  
  results.Daily[-c(1:3)] <- formatC(as.matrix(results.Daily[-c(1:3)]), format = "e", digits = 2)
  
  results.Daily.var <- results.Daily %>% filter(measure == "avg.var")
  results.Daily.returnsr <- results.Daily %>% filter(measure %in% c("avg.sr", "avg.return"))
  min.var <- apply(as.matrix(results.Daily.var[-c(1:3)]), 1, min)
  max.ret.sr <- apply(as.matrix(results.Daily.returnsr[-(1:3)]), 1, max)
  min.max <- c(max.ret.sr, min.var)
  
  results.Daily <- results.Daily %>% 
    mutate(pn = p/ windowIS,
           period = paste("n =",windowIS,", p =", p, "p/n", pn)) %>% 
    arrange(windowIS,pn, match(measure, c("avg.return", "avg.var", "avg.sr")))
  
  #rearange columns
  results.Daily <- results.Daily[c(18,1,4:16)]
  
  # Get rid of repeated periods
  for(i in nrow(results.Daily):2) {
    if(identical(results.Daily$measure[i-1], results.Daily$measure[i])) {
      results.Daily$measure[i] = ""
    }
    if(identical(results.Daily$period[i-1], results.Daily$period[i])) {
      results.Daily$period[i] = ""
    }
  }
  results.Daily <- t(results.Daily)
  
  #Plot Table
  xftbl <- xtable(results.Daily, caption = "Daily portfolio returns, risk, Sharpe Ratio (SR). Data set: December 1998 - June 2021 (5667 obs). Models that perform best in the respective period are marked bold.")
  align(xftbl) <- "c|ccc|ccc|ccc|ccc"
  
  print.xtable(xftbl, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               floating = TRUE, 
               #floating.environment = "sidewaystable",
               sanitize.text.function = function(x) {
                 x <- latexTranslate(x)
                 x <- ifelse(x %in% min.max, paste0("\\textbf{", x, "}"), x)
                 return(x)
               })
  
  #n = 250
  file.daily.results <- paste0("./results/Empirical/daily/results_daily_n250_sum.R")
  load(file.daily.results)
  
  results.Daily <- as.data.frame(result.sum)
  results.Daily <- results.Daily[-2]#remove horizon column as it is fix
  
  results.Daily[-c(1:3)] <- formatC(as.matrix(results.Daily[-c(1:3)]), format = "e", digits = 2)
  
  results.Daily.var <- results.Daily %>% filter(measure == "avg.var")
  results.Daily.returnsr <- results.Daily %>% filter(measure %in% c("avg.sr", "avg.return"))
  min.var <- apply(as.matrix(results.Daily.var[-c(1:3)]), 1, min)
  max.ret.sr <- apply(as.matrix(results.Daily.returnsr[-(1:3)]), 1, max)
  min.max <- c(max.ret.sr, min.var)
  
  results.Daily <- results.Daily %>% 
    mutate(pn = p/ windowIS,
           period = paste("n =",windowIS,", p =", p, "p/n", pn)) %>% 
    arrange(windowIS,pn, match(measure, c("avg.return", "avg.var", "avg.sr")))
  
  #rearange columns
  results.Daily <- results.Daily[c(18,1,4:16)]
  
  # Get rid of repeated periods
  for(i in nrow(results.Daily):2) {
    if(identical(results.Daily$measure[i-1], results.Daily$measure[i])) {
      results.Daily$measure[i] = ""
    }
    if(identical(results.Daily$period[i-1], results.Daily$period[i])) {
      results.Daily$period[i] = ""
    }
  }
  results.Daily <- t(results.Daily)
  
  #Plot Table
  xftbl <- xtable(results.Daily, caption = "Daily portfolio returns, risk, Sharpe Ratio (SR). Data set: December 1998 - June 2021 (5667 obs). Models that perform best in the respective period are marked bold.")
  align(xftbl) <- "c|ccc|ccc|ccc|"
  
  print.xtable(xftbl, 
               include.rownames=TRUE, 
               include.colnames=FALSE, 
               floating = TRUE, 
               #floating.environment = "sidewaystable",
               sanitize.text.function = function(x) {
                 x <- latexTranslate(x)
                 x <- ifelse(x %in% min.max, paste0("\\textbf{", x, "}"), x)
                 return(x)
               })
  }
