#############################################################################
# File name: model_hourly.R
# Author: Rama Thelagathoti
# Description: model building with hourly features
############################################################################

rownames <-  c("p1", "p2", "p3", "p4", "p5", "p6", "p7" , "p8" , "p9", "p10",
               "p11", "p12", "p13", "p14", "p15", "p16", "p17" , "p18" , "p19", "p20" ,
               "p21", "p22", "p23", "p24", "p25", "p26", "p27" , "p28" , "p29", "p30" ,
               "p31", "p32", "p33", "p34", "p35", "p36", "p37" , "p38" , "p39", "p40" ,
               "p41", "p42", "p43", "p44", "p45", "p46", "p47" , "p48" , "p49", "p50" ,
               "p51", "p52", "p53", "p54", "p55")
colnames = rownames

rownames.77 <-  c("p1", "p2", "p3", "p4", "p5", "p6", "p7" , "p8" , "p9", "p10",
                  "p11", "p12", "p13", "p14", "p15", "p16", "p17" , "p18" , "p19", "p20" ,
                  "p21", "p22", "p23", "p24", "p25", "p26", "p27" , "p28" , "p29", "p30" ,
                  "p31", "p32", "p33", "p34", "p35", "p36", "p37" , "p38" , "p39", "p40" ,
                  "p41", "p42", "p43", "p44", "p45", "p46", "p47" , "p48" , "p49", "p50" ,
                  "p51", "p52", "p53", "p54", "p55", "p56", "p57",  "p58",  "p59" ,"p60",
                  "p61", "p62", "p63", "p64", "p65", "p66", "p67",  "p68",  "p69" ,"p70",
                  "p71", "p72", "p73", "p74", "p75", "p76", "p77"
)
colnames.77 = rownames.77

############################################################################
# Function: Model_hourly_final
# Description: model building with only hourly mean and hourly sd
############################################################################
Model_hourly_final <- function(deprs.features, cont.features)
{
  
  features <- rbind(deprs.features, cont.features)
  
  preproc <- preProcess(deprs.features[,c(4:51)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:51)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(4:51)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:51)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:48) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  
  boxplot(features.st)
  
  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  
  #write.csv(features.matrix, "features.matrix_m1.csv")
  
  features.matrix.matrix <- features.matrix[,1:24]
  colnames(features.matrix.matrix) <- c("m0", "m1","m2","m3","m4","m5","m6","m7","m8",
                                        "m9","m10","m11","m12","m13","m14",
                                        "m15","m16","m17","m18","m19","m20",
                                        "m21","m22","m23")
  rownames(features.matrix.matrix) <- rownames
  features.matrix.matrix <- as.matrix(features.matrix.matrix)
  library(RColorBrewer)
  coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(features.matrix.matrix, xlab="hour", ylab="id", main="Heatmap of hourly activity",
          Rowv=NA, Colv=NA, col = coul)
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat[1:5, 1:5], main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  corrplot(cormat[1:5, 1:5], method = "number", type = "upper", number.cex = 2, tl.cex = 2,
           sig.level=0.05)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.7 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  #write.csv(cormat, "cor_matrix_n.csv")
  #write_graph(network, "edgelist_2.csv", "edgelist")
  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,4 )
  
  
}
############################################################################
# Function: model_hourly_mean
# Description: model building with only hourly mean activity
#############################################################################
model_hourly_mean <- function(deprs.features, cont.features)
{
  features <- rbind(deprs.features, cont.features)
  #normalize condition and control group separately. 4:27 f0-f23
  preproc <- preProcess(deprs.features[,c(4:27)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:27)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(4:27)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:27)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:24) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  boxplot(features.st)
  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.8 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  # with 0.7 , mcl produces all connected nodes as one cluster
  # with 0.8 - seems to be better
  # only mean is not the best predictor variable.
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,1 )
  #network_kmeans_clustering(network) 
  
  #cluster_MST_kNN(cormat)
  c <- mcl_cluster(cor_matrix,network, 2, 2, "hour-wise mean")
  return(c)
}

############################################################################
# Function: model_hourly_mean_sd
# Description: model building with only hourly mean and hourly sd
############################################################################
model_hourly_mean_sd <- function(deprs.features, cont.features)
{

  features <- rbind(deprs.features, cont.features)
  
  preproc <- preProcess(deprs.features[,c(4:51)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:51)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(4:51)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:51)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:48) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  
  boxplot(features.st)

  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  
  #write.csv(features.matrix, "features.matrix_m1.csv")
  
  features.matrix.matrix <- features.matrix[,1:24]
  colnames(features.matrix.matrix) <- c("m0", "m1","m2","m3","m4","m5","m6","m7","m8",
                                        "m9","m10","m11","m12","m13","m14",
                                        "m15","m16","m17","m18","m19","m20",
                                        "m21","m22","m23")
  rownames(features.matrix.matrix) <- rownames
  features.matrix.matrix <- as.matrix(features.matrix.matrix)
  library(RColorBrewer)
  coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(features.matrix.matrix, xlab="hour", ylab="id", main="Heatmap of hourly activity",
          Rowv=NA, Colv=NA, col = coul)
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat[1:5, 1:5], main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  corrplot(cormat[1:5, 1:5], method = "number", type = "upper", number.cex = 2, tl.cex = 2,
           sig.level=0.05)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.7 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  #write.csv(cormat, "cor_matrix_n.csv")
  #write_graph(network, "edgelist_2.csv", "edgelist")
  

  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,4 )
  
  cc <- NA
  cc$id <- 1:55
  cc$coef <- transitivity(network, type = "local")
  cc <- as.data.frame(cc)
  cc <- cc[, -1]
  cc$id <- as.factor(cc$id)
  str(cc)
  plot(cc$id, cc$coef)
  
  plot_diff_graph(network)
  
  #hs <- hub_score(network, weights=NA)$vector
  largest_cliques(network)
  cliques(network)
  
  edgelist <- as_edgelist(network, names = TRUE)
  write.csv(edgelist, "edgelist_allnodes_m1.csv")
  write.csv(cor_matrix, "cor_matrix.csv")
  
  #max clique
  cliq <- largest.cliques(network)
  g2 <- induced.subgraph(graph=network,vids=cliq[[4]])
  plot(g2)
  
  cliques(network, min=14)
  clique_num(network)
  
  n1 <- length(neighbors(network, 1))
  
  #clustering_graph(network)
  
  network_kmeans_clustering(network) 
  #network_hierarchical_clustering(network)
  
  #cluster_MST_kNN(cormat)
  #mcl produces 2 clusters
  c <- mcl_cluster(cor_matrix,network, 2, 2, "hour-wise mean & sd")
  table(as.factor(c$Cluster))
  
  return(c)

  ##########
  #find overall mean of each subject
  if (0)
  {
    meandf <- rbind(deprs.features, cont.features)
    meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
    barplot(meandf$mean, 
            names.arg=meandf$id,
            col = "seagreen")
    #write.csv(meandf, "meandf.csv")
    
    
    }
  ###############
  
  
  ##############
  #plot p2,p14,p18 which are isolated nodes vs condition group nodes
  
  x <- subset(d$day, d$id == 1 )
  y <- subset(d$day_mean, d$id == 1 )
  plot(x, y, type = "l", col = "red", xlim = c(1,max(d$day)), ylim = c(0,max(d$day_mean)),
       main = "condition vs control mean activity",
       xlab = "days", ylab = "mean activity")  
  
  for (i in 3:23) {
    
    x <- subset(d$day, d$id == i )
    y <- subset(d$day_mean, d$id == i )
    lines(x, y, type = "l", col = "red" )  
    
  }
  
  
  
  ################
  # p50 & p42 - same cluster
  p50.p42 <- features.matrix[c(50,42),]
  p50.p42 <- t(p50.p42)
  p50.p42 <- t(p50.p42)
  pal <- colorRampPalette(colors = c("pink", "lightblue"))(2)
  barplot(p50.p42, 
          legend = c("p50", "p42"), 
           beside=TRUE,
          col=pal,
          xlab="patient id", 
          ylab="mean activity", 
          main="p50 & p42 - same cluster", 
          ylim=c(0,0.5))
  abline(h=mean(p50.p42[ 1,1:24]), col="pink")
  abline(h=mean(p50.p42[ 2,1:24]), col="lightblue")
  
  # p15 & p42 - diff clusters
  p15.p42 <- features.matrix[c(15,42),]
  p15.p42 <- (t(p15.p42))
  p15.p42 <- (t(p15.p42))
  
  pal <- colorRampPalette(colors = c("pink", "lightblue"))(2)
  barplot(p15.p42, 
          legend = c("p15", "p42"), 
          beside=TRUE,
          col=pal,
          xlab="patient id", 
          ylab="mean activity", 
          main="p15 & p42 - diff clusters", 
          ylim=c(0,0.5))
  
  abline(h=mean(p15.p42[1, 1:24]), col="pink")
  abline(h=mean(p15.p42[2, 1:24]), col="lightblue")
  
  cor_matrix[50,42]
  cormat[50,42]

  cor_matrix[15,42]
  cormat[15,42]
  
  
  
  #remove h6,h15
  features.matrix.mod <- features.matrix[ ,-c(7,11,13,16,31,35,37,40)]
  cormat.mod <- cor(t(features.matrix.mod), method = "pearson")
  cormat.mod[50,42]
  # 0.7681719 decreased to 0.7501444
  
  cormat.mod[15,42]
  # 0.3250671 increased to 0.602772
  
  
  #degree distribution
  deg <- igraph::degree(network, mode="all")
  #plot(network2, vertex.size=deg*3)
  
  #degree histogram
  hist(deg, breaks=15, xlim = c(0,20), main="Histogram of node degree")
  barplot(igraph::degree(network, mode="in"), col = "skyblue", ylim = c(0,30))
  #plot(x = 1:55, y = deg, type = "s", xlab = "patient" )
  deg.dist <- igraph::degree_distribution(network, cumulative=T, mode="all")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
        xlab="Degree", ylab="Cumulative Frequency")
  

  plot(network, vertex.size=8, 
    #   vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32)), 
        vertex.color = vertex.color,
           layout=igraph::layout.fruchterman.reingold(network, niter=10000),
       main=paste("correlation graph",sep="" ))
  
  
  #greedy clustering
  fc <- fastgreedy.community(network)
  V(network)$color <- ifelse(membership(fc)==1,"red","blue")
  plot(network)
  
  #factor analysis
  fact.features <- as.matrix(features.matrix)
  cormat <- cor(fact.features, method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  
  ggcorrplot(cormat,  type = "lower", lab = TRUE, lab_size = 3,
             method="circle", colors = c("red", "white", "blue"), outline.color = "black", 
             show.legend = TRUE, show.diag = FALSE, title="Correlogram ")
  
  write.csv(cormat, "cormat.csv")
  fa1 <- factanal(covmat = cormat, factors = 10, n.obs = 55, rotation = "varimax")
  fa1
  round(cormat - (fa1$loadings %*% t(fa1$loadings) + diag(fa1$uniquenesses)), 35)
  
  fa.varimax <- factanal(covmat = cormat, n.obs = 55, factors = 35, rotation = "varimax")
  plot(fa.varimax$loadings[,1], fa.varimax$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
       xlab = "Factor 1", ylab = "Factor 3", main = "Varimax Rotation")
  abline(h = 0, v = 0)

  fa.none <- factanal(covmat = cormat, n.obs = 55, factors = 35, rotation = "none")
  plot(fa.none$loadings[,1], fa.none$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
       xlab = "Factor 1", ylab = "Factor 3", main = "no Rotation")
  abline(h = 0, v = 0)
  
  return(c)
  
}

############################################################################
# Function: model_hourly_mean_med_sd
# Description: model building with only over all mean, median, and sd
############################################################################
model_hourly_day_mean_med_sd <- function(deprs.features, cont.features)
{
  
  features <- rbind(deprs.features, cont.features)
  
  preproc <- preProcess(deprs.features[,c(52:54)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(52:54)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(52:54)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(52:54)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:3) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  boxplot(features.st)
  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  #features.matrix - col names -"mean" "med"  "sd" 
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.95 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,3 )
  #network_kmeans_clustering(network) 
  #cluster_MST_kNN(cormat)
  c <- mcl_cluster(cor_matrix,network, 2, 2, "daily mean, median & sd")
  
  return(c)
  
}

############################################################################
# Function: model_hourly_mean_sd
# Description: model building with only over all mean and sd
############################################################################
model_hourly_day_mean_sd <- function(deprs.features, cont.features)
{
  
  features <- rbind(deprs.features, cont.features)
  
  preproc <- preProcess(deprs.features[,c(52,54)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(52,54)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(52,54)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(52,54)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:2) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  boxplot(features.st)
  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  #features.matrix - col names -"mean" "med"  "sd" 
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.95 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,3 )
  #network_kmeans_clustering(network) 
  #cluster_MST_kNN(cormat)
  c <- mcl_cluster(cor_matrix,network, 2, 2, "daily mean, median & sd")
  
  return(c)
  
}

############################################################################
# Function: model_hourly_all
# Description: model building with all features
############################################################################
model_hourly_all <- function(deprs.features, cont.features)
{
 
  features <- rbind(deprs.features, cont.features)
  
  preproc <- preProcess(deprs.features[,c(4:52,54)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:52,54)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(4:52,54)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:52,54)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:50) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  boxplot(features.st)
  
  # pearson correlation
  temp <- cbind(features[ ,1], features.st)
  features.matrix <- aggregate( temp, by = list(temp$`features[, 1]`), FUN = mean)
  features.matrix <- features.matrix [ , -c(1,2)]
  features.matrix <- as.data.frame(features.matrix)
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cormat[i,j] >= 0.75 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network,2 )
  
  network_kmeans_clustering(network) 
  #cluster_MST_kNN(cormat)
  c <- mcl_cluster(cor_matrix,network, 2, 2, "hour-wise mean & sd + daily mean & sd")
  
  return(c)
  
}

