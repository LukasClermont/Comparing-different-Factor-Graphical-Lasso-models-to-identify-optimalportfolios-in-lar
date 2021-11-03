rm(list = ls())
#Get functions 
{
  file.function <- list.files(path = "./code/function")
  for(file in file.function){
    source(paste0("./code/function/", file))
  } 
}

#Matrix to save results
result.error = NULL
for(h in c(7,7.5,8,8.5,9,9.5)){
  for(iter in 1:10){
    setseed = runif(1, min = 0, max = iter*1000)
    set.seed(setseed)
    #define parameters
    u = 0.1 
    v = 0.3
    delta = 0.85
    # Compute Monte Carlo Simulation
    MC = get.MonteCarlo(delta, h, u, v, setseed = 123, dim = "high")
    R = MC$return
    # Use Monte Carlo to compute FGL
    results.FGL = fit.model(R = R, K= MC$K, model = "FGL.PCA", IS.start = 1 , IS.end = MC$T)
    Omega_hat = results.FGL$Omega
    omega_hat = results.FGL$omega
    omega = get.MVP.weights(MC$Omega)
    
    result.error <- rbind(result.error,
                          cbind(setseed, 
                          h, 
                          log2(get.norm1(Omega_hat-Omega)),
                          log2(get.norm2(Omega_hat-Omega)),
                          log2(get.norm1(omega_hat-omega)),
                          log2(get.norm2(omega_hat-omega))))
    print(paste0("h: ", h, " - iter: ", iter))
  }
}
colnames(result.error) <-c("setseed","h","omegadiff.log2.norm1", "omegadiff.log2.norm2", "weightdiff.log2.norm1", "weightdiff.log2.norm2")
sum.result <- as.data.frame(result.error) %>% group_by(h) %>% summarise(omegadiff.log2.norm1 = mean(omegadiff.log2.norm1, na.rm = TRUE),
                                                                        omegadiff.log2.norm2 = mean(omegadiff.log2.norm2, na.rm = TRUE),
                                                                        weightdiff.log2.norm1 = mean(weightdiff.log2.norm1, na.rm = TRUE),
                                                                        weightdiff.log2.norm2 = mean(weightdiff.log2.norm2, na.rm = TRUE)
                                                                        )
sum.result %>% ggplot(aes(h, omegadiff.log2.norm1))+
  geom_line()

sum.result %>% ggplot(aes(h, omegadiff.log2.norm2))+
  geom_line()

sum.result %>% ggplot(aes(h, weightdiff.log2.norm1))+
  geom_line()

sum.result %>% ggplot(aes(h, weightdiff.log2.norm2))+
  geom_line()

