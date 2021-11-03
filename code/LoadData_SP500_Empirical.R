# for scraping
library(rvest)
# blanket import for core tidyverse packages
library(tidyverse)
# tidy financial analysis 
library(tidyquant)
# tidy data cleaning functions
library(janitor)

# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table()
#create a vector of tickers
sp500tickers <- tickers[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol = case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                          Symbol == "BF.B" ~ "BF-B",
                                                          TRUE ~ as.character(Symbol)))

# save current system date to a variable
today <- Sys.Date()
# subtract 3 months from the current date
date.start = "1996-01-01"
date.end = "2021-09-30" 

get_symbols = function(ticker = "AAPL"){
  df = tq_get(ticker, from = date.start, to = date.end) %>% mutate(symbol = rep(ticker, length(date)))
}
#get symbols
symbols = sp500tickers$Symbol
tickers_df = map(symbols, get_symbols) %>% bind_rows()

df.prices.daily <- tickers_df %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "daily", col_rename = "returns")
df.prices.monthly <- tickers_df %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "returns")

df.prices.daily <- df.prices.daily %>%
  spread(key = symbol, value = returns)
df.prices.monthly <- df.prices.monthly %>%
  spread(key = symbol, value = returns)

df.prices.daily <- df.prices.daily[-c(1),]#Remove first column because its zero
df.prices.monthly <- df.prices.monthly[-c(1),]#Remove first column because its zero

#remove columns with NA for monthly data
df.prices.monthly <- df.prices.monthly[- 308,] #remove because empty
df.prices.monthly <- df.prices.monthly%>%
  select_if(~ !any(is.na(.)))

R.monthly <- df.prices.monthly[-1]
symbol.monthly <- colnames(df.prices.monthly)
date.monthly <- df.prices.monthly[1] 

R.daily <- df.prices.daily[-1]
symbol.daily <- colnames(df.prices.daily)
date.daily <- df.prices.daily[1] 

data <- NULL 
data$info = sp500tickers
data$R.monthly = R.monthly
data$date.monthly = date.monthly
data$symbol.monthly = symbol.monthly
data$R.daily = R.daily
data$date.daily = date.daily
data$symbol.daily = symbol.daily


save(data, file = "./data/raw/SP500_EmpiricalData.R")
