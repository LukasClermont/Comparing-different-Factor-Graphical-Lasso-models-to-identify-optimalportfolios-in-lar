#PLOT UNDIRECTED GRAPHICAL MODELS
#Figures 1,2,6
rm(list = ls())
#load functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}
#Figure 1: Simple undirected graph
{
  gl <- graph_from_literal(A-C-D-B, A-D-E)
  
  plot.basic <- plot.igraph(gl,
              vertex.shape="none", 
              vertex.label.font=2, vertex.label.color="gray40",
              vertex.label.cex=1, edge.color="gray85")
  
  png(file="./results/plots/Figure_1_Simple_undirected_graph.png",
      width=500, height=500)
  set.seed(123)
  plot.igraph(gl,
              vertex.shape="none", 
              vertex.label.font=2, vertex.label.color="gray40",
              vertex.label.cex=1.5, edge.color="gray85")
  dev.off()
}
#Figure 2, 3 and 6: Graphical visualization of S&P500
{
  load(file = "./data/raw/SP500_GraphVisualization")
  set.seed(123)
  R = as.matrix(data$R)
  R = R[,-396]
  S = get.cov(t(R))
  
  #2. Graphical Lasso
  lambda = 0.00001
  #Glasso
  W = S + lambda * diag(x = 1, nrow = nrow(S), ncol = ncol(S) ) #Set starting value W
  est.glasso = glasso(s = S, rho = lambda, w.init = W)
  Theta = est.glasso$wi
  #Nodewise regression
  est.MB = get.MB(S, "GIC")
  Theta = est.MB[[2]]
  
  sum(Theta == 0)/ (sum(Theta == 0) + sum(Theta != 0))
  stocknames = colnames(R)
  stockinfo = data$info
  stockinfo = stockinfo %>% filter(Symbol %in% stocknames)
  iG <- get.CovtoGraph(Theta, stocknames)
  G = iG$G
  node = as.data.frame(V(G)$name)
  names(node) = "Symbol"
  info <- left_join(node, stockinfo, by = c("Symbol"))
  GICSSectors = unique(info$`GICS Sector`)
  GICSSectors = sort(GICSSectors)
  
  col.sec <- function(sec){
    case_when( sec == GICSSectors[1] ~ "#46317a",
               sec == GICSSectors[2] ~ "#e002c6",#
               sec == GICSSectors[3] ~ "#4d6941",
               sec == GICSSectors[4] ~ "#83ad71",
               sec == GICSSectors[5] ~ "#4896a7",
               sec == GICSSectors[6] ~ "#0b5394",
               sec == GICSSectors[7] ~ "#181566",
               sec == GICSSectors[8] ~ "#c90076",
               sec == GICSSectors[9] ~ "#8e7cc3",
               sec == GICSSectors[10] ~ "#4a3341",
               sec == GICSSectors[11] ~ "#9c1046")
  }
  V(G)$label = info$Symbol
  V(G)$sector = info$`GICS Sector`
  cw = cluster_walktrap(G) # , weights = iG$weight
  plot.igraph(G,
              layout = layout.fruchterman.reingold,
              cluster = cw,
              weights = E(G)$weight,
              edge.color = edge_col(E(G)$weight),
              vertex.color = col.sec(V(G)$sector),
              vertex.label.font=2, 
              vertex.label.cex=.7, edge.color=E(G)$weight,
              edge.curved=0)
  
  E(G)$color = ifelse(E(G)$weight == 1, "#c90076", "#4896a7")
  
  edge_col <- function(x, alpha) 
    ifelse(x == 2, rgb(72/255, 150/255, 167/255,alpha = alpha), rgb(201/255, 0, 118/255,alpha = alpha))
  
  
  #Sector Plot
  png(file=paste0("./results/plots/igraph/Figure_2_Undirected_graph_with_GICS_classification.png"),
      width=1000, height=500)  
  plot.igraph(G,
              layout = layout.fruchterman.reingold,
              cluster = cw,
              weights = E(G)$weight,
              vertex.shape="circle", 
              vertex.size = 4,
              vertex.label = NA, 
              vertex.color = col.sec(V(G)$sector),
              vertex.label.cex=.7, edge.color=edge_col(E(G)$weight,alpha = 0.1),
              edge.curved=0)
  # Add a legend
  legend("topleft", legend=sort(GICSSectors),
         text.col= col.sec(sort(GICSSectors)),  cex=1)
  dev.off()
  #No lable plot
  png(file=paste0("./results/plots/igraph/Figure_6_Glasso_Undirected_graph_without_GICS_classification.png"),
      width=1000, height=500)  
  plot.igraph(G,
              cluster = cw,
              weights = E(G)$weight,
              edge.size = E(G)$weight,
              vertex.size = 0,
              vertex.shape="none", 
              vertex.color = "white",
              vertex.label = NA, 
              vertex.label.cex=1, edge.color= edge_col(E(G)$weight,alpha = 0.1),
              edge.curved=0)
  dev.off()
  
  for(cluster in 1:length(cw)){
    png(file=paste0("./results/plots/igraph/Figure_3_Undirected_graph_of_communities_",cluster,".png"),
        width=1000, height=600)
    sub.G = induced_subgraph(G, cw[[cluster]])
    plot.igraph(sub.G,
                layout = layout.fruchterman.reingold,
                vertex.shape="none", 
                vertex.label.color = col.sec(V(sub.G)$sector),
                vertex.label.font=2,
                vertex.label.cex=1, edge.color="gray85",
                edge.curved=0)
    # Add a legend
    legend("topleft", legend=sort(GICSSectors),
           text.col= col.sec(sort(GICSSectors)),  cex=1)
    dev.off()
  }
}