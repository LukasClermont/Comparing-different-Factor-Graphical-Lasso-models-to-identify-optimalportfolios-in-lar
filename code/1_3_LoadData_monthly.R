#GET MONTHLY DATAa
rm(list = ls())
day.start = "1996-01-01"
day.end = "2021-06-30"
n.stocks = 500
folder <- "./data/temp/"
#Get functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}
#Load Macroeconomic Factors
{
  macro.data <- tq_get(c("CPIAUCSL", "INDPRO", "DPCERA3M086SBEA", "M1SL", "UNRATE", "FEDFUNDS"), 
                       get = "economic.data",
                       from = day.start,
                       to = day.end) 
  
  macro.data <- macro.data %>% spread(symbol, price)
  
  colnames(macro.data) <- c("date","CPIAUCSL","INDPRO", "RPC", "M1SL", "UNRATE", "FEDFUNDS")
  macro.data$date <- gsub('-', '', macro.data$date)
  macro.data$date <- as.character(macro.data$date)
  macro.data$date <- substr(macro.data$date,1,6)
  
  macro.data <- macro.data %>%
    mutate(CPIAUCSL = ((CPIAUCSL/lag(CPIAUCSL) - 1)),
           INDPRO = ((INDPRO/lag(INDPRO) - 1)),
           RPC = ((RPC/lag(RPC) - 1)),
           M1SL = ((M1SL/lag(M1SL) - 1)),
           UNRATE = ((UNRATE/lag(UNRATE) - 1)),
           FEDFUNDS = ((FEDFUNDS/lag(FEDFUNDS) - 1)))
  macro.data <- macro.data[-1,]
}
#Load Fama French Factors (3)
{
  FF.factors <- 
    read_csv("./data/raw/F-F_Research_Data_Factors_daily.csv", 
             skip = 4) %>% 
    rename(date = X1) %>% 
    mutate_at(vars(-date), as.numeric)
  FF.factors$date <- substr(FF.factors$date,1,6)
  FF.factors <- FF.factors %>%
    group_by(date) %>%
    summarise(
    `Mkt-RF` = mean(`Mkt-RF` / 100), 
    SMB = mean(SMB / 100), 
    HML = mean(HML / 100), 
    RF = mean(RF / 100)
  )
  FF.factors <- FF.factors %>% filter(date %in% macro.data$date)
}
#Load return
{
  #get stock prices
  file.return <-  "./data/raw/SP500_EmpiricalData.R"
  load(file.return)
  infostock.data <- data$info
  R.monthly <- data$R.monthly
  date.monthly <- data$date.monthly
  date.monthly <- gsub('-', '', date.monthly$date)
  date.monthly <- as.character(date.monthly)
  symbol.monthly <- data$symbol.monthly
}
#Load index
{
  #get stock prices
  index.data <- get.index.prices.monthly(day.start, day.end)
  infoindex.data <- index.data$info
  index.data <- index.data$prices
}
#Make list with data
{
  data <- list()
  data$R <- R.monthly
  
  data$date <- date.monthly
  data$lable_stock <- symbol.monthly
  data$stockinfo <- infostock.data
  data$FF3 <- FF.factors
  data$Macro <- macro.data
  data$ER <-  data$R - data$FF3$RF #Excessreturn
  data$F_index <- index.data[-1]
  data$lable_index <- colnames(index.data[-1])
  data$indexinfo <- data$lable_index
  
  file.data <- paste0(folder, "Final_Data_Month.R")
  save(file = file.data, data)
}
