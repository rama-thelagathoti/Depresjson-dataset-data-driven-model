#############################################################################
# File name: model_daywise.R
# Author: Rama Thelagathoti
# Description: model building with daywise features
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
# Function: model_daywise_mean
# Description: model building with only daywise mean activity
#############################################################################
model_daywise_mean <- function(deprs.features, cont.features, df.scores)
{
 
  features <- rbind(deprs.features, cont.features)
  df.daywise <- features[ , c(1:2)]

  #normalize the data
  temp.df <- as.data.frame(deprs.features$mean)
  colnames(temp.df)[1] <- "mean"
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(cont.features$mean)
  colnames(temp.df)[1] <- "mean"
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand)
  
  #outlier treatment
  boxplot(stand_sep)
  stand_sep$mean <- outlier_norm(stand_sep$mean)  
  boxplot(stand_sep)
  
  df.daywise$mean <- stand_sep$mean
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = mean, values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]

  
  cor.result <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:55) {
    for (j in 1:55) {
      d <- min(df.scores$days[i], df.scores$days[j])      
      cor.result[i,j] <- cor(df.daywise.tr[1:d ,i], df.daywise.tr[1:d , j], method = "pearson")
      
    }
    
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")

  #build network
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cor.result[i,j] >= 0.5 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network, 6)

  #network_kmeans_clustering(network) 
  #cluster_MST_kNN(cor.result)
  cl <- mcl_cluster(cor_matrix,network, 2, 2, "day-wise mean")
  
  return(cl)
  
}

############################################################################
# Function: model_daywise_mean_med_sd
# Description: model building with daywise mean, median and sd activity
#############################################################################
model_daywise_mean_med_sd <- function(deprs.features, cont.features, df.scores)
{
  
  features <- rbind(deprs.features, cont.features)
  df.daywise <- features[ , c(1:2)]
  
  #normalize the data
  temp.df <- as.data.frame(deprs.features[,c(52:54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  
  temp.df <- as.data.frame(cont.features[,c(52:54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand)
  
  #outlier treatment
  boxplot(stand_sep)
  for (i in 1:3) {
    stand_sep[ , i] <- outlier_norm(stand_sep[ ,i])    
  }
  boxplot(stand_sep)
  
  df.daywise <- cbind(df.daywise,stand_sep)
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = c(mean,med,sd), values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  #df.daywise.final - col names - "mean_1"  "mean_2" .. "mean_19" 
  #"sd_1"  "sd_2" .. "sd_19" 

  
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]
  
  
  cor.result <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:55) {
    for (j in 1:55) {
      d <- min(df.scores$days[i], df.scores$days[j])      
      cor.result[i,j] <- cor(df.daywise.tr[c(1:d, 19:(18+d), 39:(38+d)) ,i], 
                             df.daywise.tr[c(1:d, 19:(18+d), 39:(38+d)) , j], method = "pearson")
      
    }
    
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")
  
  #build network
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cor.result[i,j] >= 0.5 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network, 6)
  
  #network_kmeans_clustering(network) 
  #cluster_MST_kNN(cor.result)
  c1 <- mcl_cluster(cor_matrix,network, 2, 2, "day-wise mean, median & sd")
  
  return(cl)
  
  
}


############################################################################
# Function: model_daywise_mean_sd
# Description: model building with only daywise mean and sd activity
#############################################################################
model_daywise_mean_sd <- function(deprs.features, cont.features, df.scores)
{
  
  features <- rbind(deprs.features, cont.features)
  df.daywise <- features[ , c(1:2)]
  
  #normalize the data
  temp.df <- as.data.frame(deprs.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(cont.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand)
  
  #outlier treatment
  boxplot(stand_sep)
  for (i in 1:2) {
    stand_sep[ , i] <- outlier_norm(stand_sep[ ,i])    
  }
  boxplot(stand_sep)
  
  df.daywise <- cbind(df.daywise,stand_sep)
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = c(mean,sd), values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]
  
  
  cor.result <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:55) {
    for (j in 1:55) {
      d <- min(df.scores$days[i], df.scores$days[j])      
      cor.result[i,j] <- cor(df.daywise.tr[c(1:d, 19:(18+d)) ,i], 
                             df.daywise.tr[c(1:d, 19:(18+d)) , j], method = "pearson")    
      
    }
    
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")
  
  #build network
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cor.result[i,j] >= 0.6 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network, 6)
  
  network_kmeans_clustering(network) 
  #cluster_MST_kNN(cor.result)
  clus <- mcl.r <- mcl_cluster(cor_matrix,network, 3, 3, "day-wise mean & sd")
  
  return(clus)
  
  #check correlation coefficients of patients in same cluster
  c1 <- vector()

  for (i in 1:55) {
    
    if (mcl.r$Cluster[i] == 1)
      c1 <- c(c1,cor.result[i])
      
  }
  
  
  
  #patients in same & diff cluster analysis
  df.daywise.final.mod <- df.daywise.final[, -1]
  # p26 & p39 - same cluster
  p26.p39 <- df.daywise.final.mod[c(26,39),]
  p26.p39 <- t(p26.p39)
  p26.p39 <- t(p26.p39)
  pal <- colorRampPalette(colors = c("pink", "lightblue"))(2)
  barplot(p26.p39, 
          legend = c("p26", "p39"), 
          beside=TRUE,
          col=pal,
          xlab="patient id", 
          ylab="mean activity", 
          main="p26 & p39 - same cluster", 
          ylim=c(0,0.6))
  abline(h=mean(p26.p39[ 1,1:24]), col="pink")
  abline(h=mean(p26.p39[ 2,1:24]), col="lightblue")
  
  # p46 & p19 - diff clusters
  p46.p19 <- df.daywise.final.mod[c(46,19),]
  p46.p19 <- (t(p46.p19))
  p46.p19 <- (t(p46.p19))
  
  pal <- colorRampPalette(colors = c("pink", "lightblue"))(2)
  barplot(p46.p19, 
          legend = c("p46", "p19"), 
          beside=TRUE,
          col=pal,
          xlab="patient id", 
          ylab="mean activity", 
          main="p46 & p19 - diff clusters", 
          ylim=c(0,0.6))
  
  abline(h=mean(p46.p19[1, 1:24]), col="pink")
  abline(h=mean(p46.p19[2, 1:24]), col="lightblue")
  
  
  cor_matrix[26,39]
  cor.result[26,39]
  #0.7303042
  cor_matrix[46,19]
  cor.result[46,19]
  #0.5336607
  # correlation is the strength of relation between two patients
  # 
  
  #remove h6,h15
  features.matrix.mod <- features.matrix[ ,-c(2,7,15,16,21,31,35,37,40)]
  cormat.mod <- cor(t(features.matrix.mod), method = "pearson")
  cormat.mod[26,39]
  # 0.7303042 decreased to 0.7501444
  
  cormat.mod[46,19]
  # 0.5336607 increased to 0.602772
  
  
  
  #graph analysis
  clique_num(network)
  
  
  #enrichment analysis
  df.mcl <- data.frame("id" = 1:55, "cluster" = mcl.r$Cluster)
  df.mcl$cluster <- as.factor(df.mcl$cluster)
  
  p <- ggplot(df.mcl, aes(x = id, y = cluster, color = cluster)) + geom_point() +
  geom_text(aes(label=id),hjust=0, vjust=0)
  show(p)
  
  #https://rkabacoff.github.io/datavis/Customizing.html
  #https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
  # plot total mean & sd activity of subjects in clusters
  df.mean <- df.daywise.final
  df.mean$cluster <- clus$Cluster
  df.mean$cluster <- as.factor(df.mean$cluster)
  df.mean$mean <- NA
  df.mean$sd <- NA
  
  for (i in 1:55) {
    df.mean$mean[i] <- sum(df.mean[i, 2:(df.scores$days[i]+1)])
    df.mean$sd[i] <- sum(df.mean[i, 21:(df.scores$days[i]+1+19)])  
  }
  
  ggplot(df.mean, aes(cluster, id, fill=mean)) + 
    geom_bar(stat="identity") +
    geom_text(aes(label=mean), position="stack")
  show(p)

  df.mean$mean <-   format(round(df.mean$mean, 2), nsmall = 2)
  df.mean$mean <- as.numeric(df.mean$mean)
  df.mean$sd <-   format(round(df.mean$sd, 2), nsmall = 2)
  df.mean$sd <- as.numeric(df.mean$sd)
  
  df.mean$tot <- NA
  df.mean$tot <- df.mean$mean + df.mean$sd
  df.mean$tot <- as.numeric(df.mean$tot)
  
    ggplot(df.mean, aes(x = cluster, y = id, fill = cluster)) +
    geom_point() +
    scale_y_continuous(breaks = 1:55, limits=c(0, 55)) +
    geom_text(aes(label=paste(mean,"-",sd)), hjust = -0.5)

    ggplot(df.mean, aes(x = cluster, y = id, fill = cluster)) +
    geom_point() +
    scale_y_continuous(breaks = 1:55, limits=c(0, 55)) +
    geom_text(aes(label=tot), hjust = -0.5)
    
}

############################################################################
# Function: model_daywise_mean_med_sd
# Description: model building with daywise mean, median and sd activity
#############################################################################
model_daywise_mean_med_sd_all <- function(deprs.features, cont.features,schz.features,df.scores,df.schz.info)
{
  
  features <- rbind(deprs.features, cont.features, schz.features)
  df.daywise <- features[ , c(1:2)]
  
  #normalize the data
  temp.df <- as.data.frame(deprs.features[,c(52:54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(cont.features[,c(52:54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(schz.features[,c(52:54)])
  preproc <- preProcess(temp.df, method=c("range"))
  schz_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand, schz_stand)
  
  #outlier treatment
  boxplot(stand_sep)
  for (i in 1:3) {
    stand_sep[ , i] <- outlier_norm(stand_sep[ ,i])    
  }
  boxplot(stand_sep)
  
  df.daywise <- cbind(df.daywise,stand_sep)
  write.csv(df.daywise, "daywisedf.csv")
  df.daywise %>%
    distinct() %>%
    group_by(id, day) %>%
    count()
  
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = c(mean,med,sd), values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]
  
  days <- df.scores$days
  for(k in 1:22)
  {
    days[k+55] <- df.schz.info$days[1]
  }
  cor.result <- matrix(0, nrow = length(rownames.77), ncol = length(colnames.77),  
                       dimnames = list(rownames.77, colnames.77))
  for (i in 1:length(rownames.77)) {
    for (j in 1:length(colnames.77)) {
      d <- min(days[i], days[j])      
      cor.result[i,j] <- cor(df.daywise.tr[c(1:d, 19:(18+d), 39:(38+d)) ,i], 
                             df.daywise.tr[c(1:d, 19:(18+d), 39:(38+d)) , j], method = "pearson")
      
    }
    
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")
  
  #build network
  cor_matrix <- matrix(0, nrow = length(rownames.77), ncol = length(colnames.77),  
                       dimnames = list(rownames.77, colnames.77))
  for (i in 1:length(rownames.77)) {
    for (j in 1:length(rownames.77)) {
      
      if(( cor.result[i,j] >= 0.5 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph_with_schz(network, 6)
  
  network_kmeans_clustering_with_schz(network) 
  
}

outlier_norm <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}


############################################################################
# Function: model_daywise_mean_sd
# Description: model building with only daywise mean and sd activity
#############################################################################
model_daywise_mean_sd <- function(deprs.features, cont.features, df.scores)
{
  
  features <- rbind(deprs.features, cont.features)
  df.daywise <- features[ , c(1:2)]
  
  ##########
  #find overall mean of each subject
  if (0)
  {
    meandf <- rbind(deprs.features, cont.features)
    meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
    
    #write.csv(meandf, "meandf.csv")
  
    meandf$group <- NA
    meandf$group[1:23] <- "condition"
    meandf$group[24:55] <- "control"
    meandf$group <- as.factor(meandf$group)
    ggplot(meandf, aes(group, mean)) +
    geom_boxplot(aes(fill=group)) +
      labs(title="Motor activity comparision", 
           x="Group",
           y="Mean motor activity") +
      theme(legend.title = element_blank()) +
      theme(axis.line = element_line(color = 'black')) +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ) +
      theme(axis.title = element_text(size = 15)) + 
      theme(axis.text = element_text(size = 10))  +
      theme(plot.title = element_text(size =15)) 
    
 
   }
  
  
  ###############
  #normalize the data
  temp.df <- as.data.frame(deprs.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(cont.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand)
  
  #outlier treatment
  boxplot(stand_sep)
  for (i in 1:2) {
    stand_sep[ , i] <- outlier_norm(stand_sep[ ,i])    
  }
  boxplot(stand_sep)
  
  df.daywise <- cbind(df.daywise,stand_sep)
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = c(mean,sd), values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]
  #write.csv(df.daywise.final, "df.daywise.final.csv")
  
  features.matrix.matrix <- df.daywise.tr[1:19,]
  features.matrix.matrix <- t(features.matrix.matrix)
  colnames(features.matrix.matrix) <- c("dm1","dm2","dm3","dm4","dm5","dm6","dm7","dm8",
                                        "dm9","dm10","dm11","dm12","dm13","dm14",
                                        "dm15","dm16","dm17","dm18","dm19")
  rownames(features.matrix.matrix) <- rownames
  features.matrix.matrix <- as.matrix(features.matrix.matrix)
  library(RColorBrewer)
  coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(features.matrix.matrix, xlab="day", ylab="id", main="Heatmap of day-wise activity",
          Rowv=NA, Colv=NA, col = coul)
  
  
  cor.result <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:55) {
    for (j in 1:55) {
      d <- min(df.scores$days[i], df.scores$days[j])      
      #d  <- 6
      cor.result[i,j] <- cor(df.daywise.tr[c(1:d, 19:(18+d)) ,i], 
                             df.daywise.tr[c(1:d, 19:(18+d)) , j], method = "pearson")    
      
    }
    
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")
  
  #build network
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cor.result[i,j] >= 0.55 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  

  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network, 6)
  

  #network_kmeans_clustering(network) 
  #network_hierarchical_clustering(network)
  #cluster_MST_kNN(cor.result)
  c1 <- mcl_cluster(cor_matrix,network, 2, 2, "day-wise mean & sd")
  
  return(c1)
  
  
  
  
}

############################################################################
# Function: feature_selectin_daywise_mean_sd
# Description: model building with only daywise mean and sd activity
#############################################################################
feature_selectin_daywise_mean_sd  <- function(deprs.features, cont.features, df.scores)
{
  
}

############################################################################
# Function: model_daywise_hybrid
#hourly features 48 + day features 38
#############################################################################
model_daywise_mean_sd <- function(deprs.features, cont.features, df.scores)
{
  
  features <- rbind(deprs.features, cont.features)
  df.daywise <- features[ , c(1:2)]
  
  #normalize the data
  temp.df <- as.data.frame(deprs.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cond_stand <- predict(preproc, temp.df)
  temp.df <- as.data.frame(cont.features[,c(52,54)])
  preproc <- preProcess(temp.df, method=c("range"))
  cont_stand <- predict(preproc, temp.df)
  stand_sep <- rbind(cond_stand, cont_stand)
  #outlier treatment
  boxplot(stand_sep)
  for (i in 1:2) {
    stand_sep[ , i] <- outlier_norm(stand_sep[ ,i])    
  }
  boxplot(stand_sep)
  df.daywise <- cbind(df.daywise,stand_sep)
  df.daywise.final <- df.daywise %>%
    pivot_wider(names_from = day, values_from = c(mean,sd), values_fill = 0)
  df.daywise.final <- as.data.frame(df.daywise.final)
  df.daywise.tr <- as.data.frame(t(df.daywise.final))
  df.daywise.tr <- df.daywise.tr[-1,]
  
  preproc <- preProcess(deprs.features[,c(4:51)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:51)])
  preproc <- preProcess(cont.features[,c(4:51)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:51)])
  features.st <- rbind(deprs.features.st, cont.features.st)
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
  
  
  
  merge.df <- cbind((df.daywise.final), (features.matrix))
  merge.df <- merge.df[, -1]
  merge.df <- t(merge.df)
  merge.df <- as.data.frame(merge.df)
  cor.result <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:55) {
    for (j in 1:55) {
      d <- min(df.scores$days[i], df.scores$days[j])      
      cor.result[i,j] <- cor(merge.df[c(1:d, 19:(18+d), 49:86) ,i], 
                             merge.df[c(1:d, 19:(18+d),49:86) , j], method = "pearson")    
      
    }
  }
  hist(cor.result)
  corrplot::corrplot(cor.result, main="Correlation Plot for Numerical Variables", method="number")
  
  #build network
  cor_matrix <- matrix(0, nrow = length(rownames), ncol = length(colnames),  dimnames = list(rownames, colnames))
  for (i in 1:length(rownames)) {
    for (j in 1:length(colnames)) {
      
      if(( cor.result[i,j] >= 0.55 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot_corr_graph(network, 6)
  
  network_kmeans_clustering(network) 
  #cluster_MST_kNN(cor.result)
  clus <- mcl.r <- mcl_cluster(cor_matrix,network, 3, 3, "day-wise mean & sd")
  
  return(clus)
  

}


