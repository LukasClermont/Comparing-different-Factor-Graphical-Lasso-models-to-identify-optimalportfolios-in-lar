#LOAD AND PREPARE MONTHLY DATA
rm(list = ls())
day.start = "1996-01-01"
day.end = "2021-09-30" 
folder <- "./data/temp/"
#Get functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}
#Load return
{
  file.return <-  "./data/temp/SP500_EmpiricalData.R"
  load(file.return)
  infostock.data <- data$info
  R.daily <- data$R.daily
  date.daily <- data$date.daily
  date.daily <- gsub('-', '', date.daily$date)
  date.daily <- as.character(date.daily)
  symbol.daily <- data$symbol.daily
}
#Load three Fama-French factors
{
  FF.factors <- 
    read_csv("./data/raw/F-F_Research_Data_Factors_daily.csv", 
             skip = 4) %>% 
    rename(date = X1) %>% 
    mutate_at(vars(-date), as.numeric)
  FF.factors <- FF.factors %>%
    group_by(date) %>%
    summarise(
      `Mkt-RF` = mean(`Mkt-RF` / 100), 
      SMB = mean(SMB / 100), 
      HML = mean(HML / 100), 
      RF = mean(RF / 100)
    )
  FF.factors <- FF.factors %>% filter(date %in% date.daily)
}
#Load index
{
  #get stock prices
  index.data <- get.index.prices.daily(day.start, day.end)
  infoindex.data <- index.data$info
  index.data <- index.data$prices
}
#Make list with stock return and factors
{
  #check if dates exist in data sets
  R.daily$date <- date.daily
  R.daily <- R.daily %>% filter(date %in% FF.factors$date & date %in% index.data$date)
  FF.factors <- FF.factors %>% filter(date %in% R.daily$date)
  index.data <- index.data %>% filter(date %in% R.daily$date)
  
  data <- list()
  data$R <- R.daily %>% select(-date)
  data$date <- R.daily$date
  data$lable_stock <- symbol.daily
  data$stockinfo <- infostock.data
  data$FF3 <- FF.factors %>% select(-date)
  data$ER <-  data$R - data$FF3$RF #Excessreturn
  data$F_index <- index.data %>% select(-date)
  data$lable_index <- colnames(index.data[-1])
  data$indexinfo <- data$lable_index
  
  file.data <- paste0(folder, "Final_Data_Day_NEW.R")
  load(file.data)
  file.data <- paste0(folder, "Final_Data_Day_1.R")
  data1 <- data[1:4]
  save(file = file.data, data1)
  file.data <- paste0(folder, "Final_Data_Day_2.R")
  data2 <- data[5:9]
  save(file = file.data, data[1:4])
}
