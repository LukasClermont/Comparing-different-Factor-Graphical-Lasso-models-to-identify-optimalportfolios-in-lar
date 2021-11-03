get.stock.prices.daily <- function(day.start, day.end, nr.tickers){
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
