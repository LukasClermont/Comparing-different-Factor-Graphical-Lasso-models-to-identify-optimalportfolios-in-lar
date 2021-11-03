library(RPostgres)
library(tidyverse)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='lclermo1',
                  password = readline("Password:  "))

