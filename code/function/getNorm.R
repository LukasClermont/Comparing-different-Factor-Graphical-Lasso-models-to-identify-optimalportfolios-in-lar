get.norm1 <- function(x) max(colSums(abs(x)))

get.norm2 <- function(x) sqrt(max(eigen(x %*% t(x))$values))