
date.start = "2021-01-01"
date.end = "2021-06-31" 

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

get_symbols = function(ticker = "AAPL"){
  df = tq_get(ticker, from = date.start, to = date.end) %>% mutate(symbol = rep(ticker, length(date)))
}
#get symbols
symbols = sp500tickers$Symbol
tickers_df = map(symbols, get_symbols) %>% bind_rows()

df.prices <- tickers_df %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "daily", col_rename = "returns")

df.prices <- df.prices %>%
  spread(key = symbol, value = returns)
df.prices <- df.prices[-c(1,20),]

#remove columns with NA
df.prices <- df.prices%>%
  select_if(~ !any(is.na(.)))

df.prices$date <- gsub('-', '', df.prices$date)
df.prices$date <- as.character(df.prices$date)

tickers <- unique(colnames(df.prices[-1]))

data <- NULL 
data$info = sp500tickers
data$R = df.prices[-1]
data$date = df.prices[1]

save(data, file = "./data/temp/SP500_GraphVisualization.R")
load(file = "./data/temp/SP500_GraphVisualization")
