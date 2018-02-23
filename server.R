shinyServer(function(input, output,session){
  
  output$myPlot <- renderPlot({
    
    alpha <- input$alpha
    eps <- input$epsilon
    corr <- input$Correlation
    method <- input$Method
    
    #if(corr == "Pearson" & method == "Asymptotic" ){
    
    if(method == "Asymptotic" ){
      
      if(corr == "Pearson"){
    
    
    
    L_P <- tanh(Z_Hat_P - qnorm(1-(alpha/(2*m)))/sqrt(length(STOCK)-3))
    U_P <- tanh(Z_Hat_P + qnorm(1-(alpha/(2*m)))/sqrt(length(STOCK)-3))
    Edges_P <- matrix(NA, nrow = length(STOCK), ncol = length(STOCK))
    for (i in 1:length(STOCK)){
      for (j in 1:length(STOCK)){
        if ( (L_P[i,j]>eps) | (U_P[i,j]<(-eps))){
          Edges_P[i,j] <- 1
        }
        else{
          Edges_P[i,j] <- 0
        }
      }
    }
    #Pearson
    net_P=graph.adjacency(Edges_P,mode="undirected",weighted=NULL,diag=FALSE)
    colors <- c("green","red","orchid","blue","yellow","orange","black","grey","brown","purple")
    for ( i in 10:1){
      V(net_P)[ V(net_P) <= i*10 ]$color <- colors[i]
    }
    plot.igraph(net_P,vertex.label=NA,layout=layout.fruchterman.reingold,
                vertex.size=5, main= "Marginal Correlation Graph - Pearson - Asymptotic")
    legend(x = "topleft", GICS ,col = colors, bty="n", cex=0.7, lty=1)
    
    }
    
    else{ 
      #if(corr == "Spearman" & method="Asymptotic") {
      
      #Spearman
      L_K <- tanh(Z_Hat_K - qnorm(1-(alpha/2))/sqrt(length(STOCK)-3))
      U_K <- tanh(Z_Hat_K + qnorm(1-(alpha/2))/sqrt(length(STOCK)-3))
      
      eps = 0.1
      Edges_K <- matrix(NA, nrow = length(STOCK), ncol = length(STOCK))
      for (i in 1:length(STOCK)){
        for (j in 1:length(STOCK)){
          if ( (L_K[i,j]>eps) | (U_K[i,j]<(-eps))){
            Edges_K[i,j] <- 1
          }
          else{
            Edges_K[i,j] <- 0
          }
        }
      }
      
      #Spearman
      net_K=graph.adjacency(Edges_K,mode="undirected",weighted=NULL,diag=FALSE)
      colors <- c("green","red","orchid","blue","yellow","orange","black","grey","brown","purple")
      for ( i in 10:1){
        V(net_K)[ V(net_K) <= i*10 ]$color <- colors[i]
      }
      plot.igraph(net_K,vertex.label=NA,layout=layout.fruchterman.reingold,
                  vertex.size=5, main= "Marginal Correlation Graph - Spearman - Asymptotic")
      legend(x = "topleft", GICS ,col = colors, bty="n", cex=0.7, lty=1)
      }
    
    }
    
  else{
    if(corr == "Pearson"){
      #Pearson
      t_al_P = quantile(F_hat_P,1-alpha) 
      
      Sim_Conf_Set.low_P = R_hat_P - (t_al_P/(sqrt(length(STOCK))))
      
      Sim_Conf_Set.upp_P = R_hat_P + (t_al_P/(sqrt(length(STOCK))))
      
      Edges_P_B <- matrix(NA, nrow = length(STOCK), ncol = length(STOCK))
      for (i in 1:length(STOCK)){
        for (j in 1:length(STOCK)){
          if ( ((Sim_Conf_Set.low_P[i,j]>=-eps) & (Sim_Conf_Set.low_P[i,j]<=eps)) | ((Sim_Conf_Set.upp_P[i,j]>=-eps) & (Sim_Conf_Set.upp_P[i,j]<=eps)) | ((Sim_Conf_Set.low_P[i,j]<=-eps) & (Sim_Conf_Set.upp_P[i,j]>=eps))){
            Edges_P_B[i,j] <- 0
          }
          else{
            Edges_P_B[i,j] <- 1
          }
        }
      }
      
      #Pearson
      net_P_B=graph.adjacency(Edges_P_B,mode="undirected",weighted=NULL,diag=FALSE)
      colors <- c("green","red","orchid","blue","yellow","orange","black","grey","brown","purple")
      for ( i in 10:1){
        V(net_P_B)[ V(net_P_B) <= i*10 ]$color <- colors[i]
      }
      plot.igraph(net_P_B,vertex.label=NA,vertex.size=5,layout=layout.fruchterman.reingold,
                  main= "Marginal Correlation Graph - Pearson - Bootstrap")
      legend(x = "topleft", GICS ,col = colors, bty="n", cex=0.7, lty=1)
      
      
    }
    else{
      #Spearman
      t_al_K = quantile(F_hat_K,1-alpha)
      
      Sim_Conf_Set.low_K = R_hat_P - (t_al_K/(sqrt(length(STOCK))))
      
      Sim_Conf_Set.upp_K = R_hat_P + (t_al_K/(sqrt(length(STOCK))))
      
      Edges_K_B <- matrix(NA, nrow = length(STOCK), ncol = length(STOCK))
      for (i in 1:length(STOCK)){
        for (j in 1:length(STOCK)){
          if ( ((Sim_Conf_Set.low_K[i,j]>=-eps) & (Sim_Conf_Set.low_K[i,j]<=eps)) | ((Sim_Conf_Set.upp_K[i,j]>=-eps) & (Sim_Conf_Set.upp_K[i,j]<=eps)) | ((Sim_Conf_Set.low_K[i,j]<=-eps) & (Sim_Conf_Set.upp_K[i,j]>=eps))){
            Edges_K_B[i,j] <- 0
          }
          else{
            Edges_K_B[i,j] <- 1
          }
        }
      }
      
      #Spearman
      net_K_B=graph.adjacency(Edges_K_B,mode="undirected",weighted=NULL,diag=FALSE)
      colors <- c("green","red","orchid","blue","yellow","orange","black","grey","brown","purple")
      for ( i in 10:1){
        V(net_K_B)[ V(net_K_B) <= i*10 ]$color <- colors[i]
      }
      plot.igraph(net_K_B,vertex.label=NA,vertex.size=5,layout=layout.fruchterman.reingold,
                  main= "Marginal Correlation Graph - Spearman - Bootstrap")
      legend(x = "topleft", GICS ,col = colors, bty="n", cex=0.7, lty=1)
      
    }
    
    
  }
 

  
})
})