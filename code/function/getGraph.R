get.CovtoGraph <- function(Theta, stocknames, G = NULL, weight = NULL){
  diag(Theta) <- 0
  if(all(Theta == 0)== FALSE){
    stockgraph <- Theta
    colnames(stockgraph) <- stocknames
    rownames(stockgraph) <- stocknames
    stockgraph <- as.data.frame(stockgraph)
    # adjacency <- abs(Theta) > 1E-4; diag(adjacency) <- 0
    # G <- graph.adjacency(adjacency, mode='undirected')
    stockgraph$From <- stocknames
    stockgraph <- stockgraph %>%
      gather("To", "weight", -From) %>%
      filter(weight != 0, From != To)

    stockgraph <- unique(stockgraph)
    stockgraph <- stockgraph %>% mutate(weight = case_when(
      weight > 0 ~1,
      weight < 0 ~2
    ))
    G = graph_from_data_frame(stockgraph, directed = F)
  }
  return(list(G = G, weight = stockgraph$weight))
} 
