#PLOT UNDIRECTED GRAPHICAL MODELS
#Figures 1,2,3,6
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
  set.seed(123)
  gl <- graph_from_literal(A-C-D-B, A-D-E)
  
  plot.basic <- plot.igraph(gl,
              vertex.shape="none", 
              vertex.label.font=2, vertex.label.color="gray40",
              vertex.label.cex=1, edge.color="gray85")
  
  png(file="./figures/Figure_1_Simple_undirected_graph.png",
      width=500, height=500)
  set.seed(123)
  plot.igraph(gl,
              vertex.shape="none", 
              vertex.label.font=2, vertex.label.color="gray40",
              vertex.label.cex=1.5, edge.color="gray85")
  dev.off()
}
#Function
{
#define colors
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

edge_col <- function(x, alpha) 
  ifelse(x == 2, rgb(72/255, 150/255, 167/255,alpha = alpha), rgb(201/255, 0, 118/255,alpha = alpha))
}
#Figure 2, 3 and 6: Graphical visualization of S&P500
{
  #load data
  load(file = "./data/temp/Final_Data_Day_1.R")
  load(file = "./data/temp/Final_Data_Day_2.R")
  data <- c(data1, data2)
  rm(data1, data2)
  set.seed(123)
  R = t(as.matrix(data$R[5544:5667,]))
  R <- R[rowSums(is.na(R)) == 0,]
  stocknames = rownames(R)
  stockinfo = data$stockinfo
  stockinfo = stockinfo %>% filter(Symbol %in% stocknames)
  
  #make graph frot both models
  model <- c("Glasso", "MB")
  
  for(choice in model){
    set.seed(123)
    #Glasso
    if(choice == "Glasso"){
      est.glasso <- get.GlassoEBIC(R, lambda = 0.00001)
      Theta = est.glasso$Omega
    }
    #Nodewise regression
    if(choice == "MB"){
      S <- get.cov(R)
      est.MB = get.MB(S, "GIC")
      Theta = est.MB[[2]]
    }
    #Transform covariance matrix to undirected graph
    iG <- get.CovtoGraph(Theta, stocknames)
    G = iG$G #define graph
    node = as.data.frame(V(G)$name)
    names(node) = "Symbol"
    info <- left_join(node, stockinfo, by = c("Symbol"))
    #include sector information in the graph
    GICSSectors = unique(info$`GICS Sector`)
    GICSSectors = sort(GICSSectors)
    V(G)$label = info$Symbol
    V(G)$sector = info$`GICS Sector`
    cw = cluster_walktrap(G)
    
    #Sector Plot
    png(file=paste0("./figures/Figure_2_",choice,"_Undirected_graph_with_GICS_classification.png"),
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
    png(file=paste0("./figures/Figure_6_",choice,"_Undirected_graph_without_GICS_classification.png"),
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
      png(file=paste0("./figures/Figure_",choice,"_Undirected_graph_of_communities_",cluster,".png"),
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
}
  