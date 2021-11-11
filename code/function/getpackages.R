#LOAD REQUIRED PACKAGES
packages <- c("tidyverse", #Data analysis 
              "tidyquant", #Import from Yahoo Fianance
              "MASS","huge", "Rcpp",#Monte Carlo
              "glasso",#Graphical Lasso package by Friedmann et al.
              "ggraph", "igraph", "tidygraph",#Visualization
              "glmnet",#for LASSO
              "rvest", "janitor"# for scraping
              )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

