# IMPORT DAILY AND MONTHLY STOCK AND INDEX PRICES FROM YAHOO FINANCE

get.stock.prices.daily <- function(day.start, day.end, nr.tickers){
  # Input:
  # day.start -- start day for the requested period (e.g. "2021-01-01")
  # day.end -- end day for the requested period (e.g. "2021-07-01")
  # nr.tickers -- number of tickers
  #
  # Output:
  # list of prices and stock info
  
  exchange = tq_exchange("NASDAQ")
  exchange = exchange %>% 
    filter(country == "United States", 
           is.na(industry) == FALSE, industry != "", 
           ipo.year <= year(day.start),
    )
  tickers <- exchange$symbol
  #sample random names of tickers
  if(nr.tickers < length(tickers)){
    tickers <- sample(tickers, nr.tickers)
  }
  
  prices <- tq_get(tickers,
                   from = day.start,
                   to = day.end,
                   get = "stock.prices") 
  tickers <- unique(prices$symbol)
  
  df.prices <- prices %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "daily", col_rename = "returns")
  
  df.prices <- df.prices %>%
    spread(key = symbol, value = returns)
  
  #remove columns with NA
  df.prices <- df.prices%>%
    select_if(~ !any(is.na(.)))
  
  df.prices$date <- gsub('-', '', df.prices$date)
  df.prices$date <- as.character(df.prices$date)
  
  tickers <- unique(colnames(df.prices[-1]))
  exchange <- exchange %>% filter(symbol %in% tickers)
  
  return(list(prices = df.prices, info = exchange))
}

get.index.prices.daily <- function(day.start, day.end){
  # Input:
  # day.start -- start day for the requested period (e.g. "2021-01-01")
  # day.end -- end day for the requested period (e.g. "2021-07-01")
  #
  # Output:
  # list of prices and stock info
  tickers <-  c("XLE", "XLV", "XLF", "XLI", "XLU", "XLK")
  names <-  c("Energy", "Health", "FiancialServices", "Industrials", "Utilities", "Technologies")
  
  prices <- tq_get(tickers,
                   from = day.start,
                   to = day.end,
                   get = "stock.prices")
  
  df.index <- prices %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "daily", col_rename = "returns")
  
  df.index <- df.index %>%
    spread(key = symbol, value = returns)
  df.index$date <- gsub('-', '', df.index$date)
  df.index$date <- as.character(df.index$date)
  
  return(list(prices = df.index, info = cbind(tickers,names)))
}

get.stock.prices.monthly <- function(day.start, day.end, nr.tickers){
  # Input:
  # day.start -- start day for the requested period (e.g. "2021-01-01")
  # day.end -- end day for the requested period (e.g. "2021-07-01")
  # nr.tickers -- number of tickers
  #
  # Output:
  # list of prices and stock info
  exchange = tq_exchange("NASDAQ")
  exchange = exchange %>% 
    filter(country == "United States", 
           is.na(industry) == FALSE, industry != "", 
           ipo.year <= year(day.start),
    )
  tickers <- exchange$symbol
  #sample random names of tickers
  if(nr.tickers < length(tickers)){
    tickers <- sample(tickers, nr.tickers)
  }
  
  prices <- tq_get(tickers,
                   from = day.start,
                   to = day.end,
                   get = "stock.prices") 
  tickers <- unique(prices$symbol)
  
  df.prices <- prices %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "returns")
  
  df.prices <- df.prices %>%
    spread(key = symbol, value = returns)
  
  #remove columns with NA
  df.prices <- df.prices%>%
    select_if(~ !any(is.na(.)))
  
  df.prices$date <- gsub('-', '', df.prices$date)
  df.prices$date <- as.character(df.prices$date)
  df.prices$date <- substr(df.prices$date,1,6)
  
  tickers <- unique(colnames(df.prices[-1]))
  exchange <- exchange %>% filter(symbol %in% tickers)
  
  return(list(prices = df.prices, info = exchange))
}

get.index.prices.monthly <- function(day.start, day.end){
  # Input:
  # day.start -- start day for the requested period (e.g. "2021-01-01")
  # day.end -- end day for the requested period (e.g. "2021-07-01")
  # nr.tickers -- number of tickers
  #
  # Output:
  # list of prices and stock info
  tickers <-  c("XLE", "XLV", "XLF", "XLI", "XLU", "XLK")
  names <-  c("Energy", "Health", "FiancialServices", "Industrials", "Utilities", "Technologies")
  
  prices <- tq_get(tickers,
                   from = day.start,
                   to = day.end,
                   get = "stock.prices")
  
  df.index <- prices %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "returns")
  
  df.index <- df.index %>%
    spread(key = symbol, value = returns)
  df.index$date <- gsub('-', '', df.index$date)
  df.index$date <- as.character(df.index$date)
  df.index$date <- substr(df.index$date,1,6)
  
  return(list(prices = df.index, info = cbind(tickers,names)))
}

