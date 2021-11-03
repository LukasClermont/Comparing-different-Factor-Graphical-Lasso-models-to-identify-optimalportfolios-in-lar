get.cov.plot <- function(Cov) {
  df <- as.data.frame(Cov) %>% 
    mutate(stocka = rownames(Cov))
  
  df <- df %>% gather("stockb", "covariance", -stocka)
  
  ggplot.cov <- ggplot(data = df, aes(x=stocka, y=stockb, fill=covariance)) + 
    geom_tile()
  return(ggplot.cov)
}