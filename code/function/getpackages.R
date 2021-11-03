packages <- c("tidyverse", 
              "MASS", 
              "huge", "Rcpp",#Monte Carlo
              "tidyquant",
              "glasso",
              "DWLasso",
              "ggraph",
              "igraph",
              "tidygraph",
              "glmnet",#for LASSO
              "rmgarch"#DCC Model
              )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

