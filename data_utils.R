#############################################################################
# File name: data_utils.R
# Author: Rama Thelagathoti
# Description: utility functions
############################################################################
library(dplyr)

grps <- df.chars$type
mean <- df.chars$mean
AvgMean <- c(196,284)
grp <- c("Condition", "Control")
df <- data.frame(grps, mean)
df$grps <- as.factor(df$grps)
ggplot(df, aes(x=grps, y=mean, fill = grps)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "#e74c3c", "#2ecc71"))

##################################################################
# outlier detection and normalizing
##################################################################
outlier_norm <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}

##################################################################
# Hierarchical clustering method applied on the correlation graph
##################################################################

network_hierarchical_clustering <- function(network)
{  
  A = get.adjacency(network, sparse=FALSE)
  # cosine similarity
  S = cosine(A)
  # distance matrix
  D = 1-S
  # distance object
  d = as.dist(D)
  # average-linkage clustering method
  cc = hclust(d, method = "average")
  # plot dendrogram
  plot(cc)
  # draw blue borders around clusters
  clusters.list = rect.hclust(cc, k = 2, border="blue")
}

##################################################################
# k-means clustering method applied on the correlation graph
##################################################################

network_kmeans_clustering <- function(network)
{
  # k-means clustering
  A = get.adjacency(network, sparse=FALSE)
  
  k <- 2
  km <- kmeans(A, centers = k, nstart = 25)
  km$tot.withinss
  km_plot <- NA
  km_plot$id <- 1:55
  km_plot$cluster <- km$cluster
  km_plot <- as.data.frame(km_plot)
  km_plot <- km_plot[,-1]
  p <- ggplot(km_plot, aes(x = 1:55, y = cluster, color = cluster)) + geom_point() +
    geom_text(aes(label=id),hjust=0, vjust=0)
  show(p)
  
  sil <- silhouette(km$cluster, dist(A))
  summary(sil)
  if(0)
  {
      fviz_cluster(km, data = km_plot)
      
      # function to compute total within-cluster sum of square 
      wss <- function(k) {
        kmeans(A, k, nstart = 10 )$tot.withinss
      }
      
      # Compute and plot wss for k = 1 to k = 15
      k.values <- 1:15
      
      # extract wss for 2-15 clusters
      wss_values <- map_dbl(k.values, wss)
      
      plot(k.values, wss_values,
           type="b", pch = 19, frame = FALSE, 
           xlab="Number of clusters K",
           ylab="Total within-clusters sum of squares")
      
      #http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determiningthe-optimal-number-of-clusters-3-must-know-methods/#average-silhouette-method
      
      fviz_nbclust(A, kmeans, method = "wss")+
        geom_vline(xintercept = 4, linetype = 2)+
        labs(subtitle = "Elbow method")
      
      fviz_nbclust(A, kmeans, method = "silhouette") +
        labs(subtitle = "Silhouette method")

      fviz_nbclust(A, kmeans, method = "gap_stat") +
        labs(subtitle = "Gap statistic method")
      
      clusterlouvain <- cluster_louvain(network)
      plot(network, vertex.color=rainbow(4, alpha=0.6)[clusterlouvain$membership])
      
    
      imc <- cluster_infomap(network)
      plot(network, vertex.color=rainbow(4, alpha=0.6)[imc$membership])
      
      plot(imc, network)
  }  
  #confusion matrix with known groups
  km_plot$group <- NA
  km_plot$group[1:23] <- 1
  km_plot$group[24:55] <- 2
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "2")

  km_plot$group[1:23] <- 2
  km_plot$group[24:55] <- 1
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "1")
  
}

##################################################################
# clustering method applied on the correlation graph with metric
##################################################################

clustering_graph <- function(network)
{
  # k-means clustering
  A = get.adjacency(network, sparse=FALSE)
  
  n_clust<-fviz_nbclust(A, kmeans, method = "silhouette",k.max = 10)
  show(n_clust)
  n_clust<-n_clust$data
  max_cluster<-as.numeric(n_clust$clusters[which.max(n_clust$y)])
  
  k <- max_cluster
  km <- kmeans(A, centers = k, nstart = 25)
  km$tot.withinss
  km_plot <- NA
  km_plot$id <- 1:55
  km_plot$cluster <- km$cluster
  km_plot <- as.data.frame(km_plot)
  km_plot <- km_plot[,-1]
  
  p <- fviz_cluster(km, data = km_plot)
  show(p)
  
  #clusterlouvain <- cluster_louvain(network)
  #plot(network, vertex.color=rainbow(4, alpha=0.6)[clusterlouvain$membership])
  #imc <- cluster_infomap(network)
  #plot(network, vertex.color=rainbow(4, alpha=0.6)[imc$membership])
  #plot(imc, network)
  
  #confusion matrix with known groups
  km_plot$group <- NA
  km_plot$group[1:23] <- 1
  km_plot$group[24:55] <- 2
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "2")
  
  km_plot$group[1:23] <- 2
  km_plot$group[24:55] <- 1
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "1")
  
}

node_analysis <- function(cor_matrix, network, exp, infl, features)
{
  
  
  #exp = 3; infl = 3 ; features = "cluster"
  exp = 2; infl = 2 ; features = "cluster"
  
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  #deg <- igraph::degree(network, mode="all")
  #barplot(deg, type = "l")

  #find degree of each node and stote it in a vector
  deg <- igraph::degree(network, mode="all")
  #plot(x = 1:55, y = deg, type = "b", xlab = "patient" )
  #axis(1,at=1:55,labels=1:55)
  #grid()
  
  t <- as.data.frame(deg)
  degree <- t$deg
  
  
  #find edges between nodes
  #https://stackoverflow.com/questions/54929802/extracting-an-edge-list-with-conditions-from-an-igraph-object-in-r
  #E(tempnet)[V(tempnet)[name== "p44"] %--% V(tempnet)[name=="p55"]]
  #V(network)[inc(E(network)[1])][3]
  tempnet <- network
  V(tempnet)$cluster <- mcl.res$Cluster
  E(tempnet)[V(tempnet)[name== "p3" ] %--% V(tempnet)]
  
  print(E(tempnet)[V(tempnet)[name== "p1" ] %--% V(tempnet)])
  
  for (i in 1:55)
  {
    #print(E(tempnet)[V(tempnet)[name== paste("p",i,sep = "") ] %--% V(tempnet)])
  }
  
  #find intra-inter cluster edges
  
  
  node.score <- data.frame(matrix(ncol = 7, nrow = 55))
  cols <- c("id","cluster", "deg", "intra", "inter", "score","ccoef")
  colnames(node.score) <- cols
  node.score$id <- 1:55
  node.score$cluster <- mcl.res$Cluster
  node.score$deg <- igraph::degree(network)
  node.score$deg [is.na(node.score$deg)] <- 0
  
  node.score[is.na(node.score)] = 0
  node.score$ccoef <- transitivity(network, type = "local")
  node.score$ccoef [is.na(node.score$ccoef)] <- 0
  
  net.matrix = get.adjacency(network, sparse=FALSE)
    # since its a 55x55 matrix , use 2 loop variable i & j
    for (i in 1:55) {

    for (j in 1:55) {
      # if edge present & not a loop
      if((net.matrix[i,j] == 1) && (i != j))
      {
        # same cluster then intra else inter edge
        if(node.score$cluster[i] == node.score$cluster[j])
        {
          node.score$intra[i] = node.score$intra[i] + 1
        }
        else
        {
          node.score$inter[i] = node.score$inter[i] + 1
        }
      }
      
    }
  }
  
  node.score$score <- node.score$inter/node.score$intra
  node.score$score [is.na(node.score$score)] <- 0
  
 
  df.chars <-  cbind(node.score, df.scores)
  df.chars$avg <- colMeans(rbind(df.chars$madrs1, df.chars$madrs2), na.rm=TRUE)
  
  df.chars$grp <- NA
  for (i in 1:23) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- 1
    else
      df.chars$grp[i] <- 2
  }
  
  for (i in 24:55) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- 4
    else
      df.chars$grp[i] <- 3
  }
  
  df.chars$grp <-as.factor(df.chars$grp) 
  
  
  # get mean 
  meandf <- rbind(deprs.features, cont.features)
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[24] <- "mean"
  
  
  df.chars.simple <- df.chars[, -c(8,12:17,20:21,23)]
  
  write.csv(df.chars.simple, "dfcharssimple.csv")
  
  ggplot(df.chars, aes(x = as.factor(id), y = score, fill = type)) +
    geom_bar(stat="identity", width = 0.5) +
    ggtitle("Score of each person") +
    xlab("subject id") + ylab("score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 

  show(p)
  
  
  df.grp1 <- subset(df.chars, score == 0 & id < 24 & id !=2 & id != 14 & id != 18)
  df.grp2 <- subset(df.chars, score > 0 & id < 24 & id !=2 & id != 14 & id != 18)
  df.grp3 <- subset(df.chars, score > 0 & id > 23 & id != 44)
  df.grp4 <- subset(df.chars, score == 0 & id > 23 & id != 44)
  #df.grp5 <- subset(df.chars, id == 2 | id == 14 | id == 18 | id == 44)
  
  df.grp <- rbind(df.grp1, df.grp2, df.grp3, df.grp4)
  df.grp$id <- factor(df.grp$id, levels = df.grp$id)
  
  ggplot(df.grp, aes(x = id, y = mean, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green","blue")) +
    ggtitle("mean activity across groups") +
    xlab("id") + ylab(" mean activity") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) +
    theme_bw()
  
  
  df.grp.temp <- rbind(df.grp1, df.grp2)
  df.grp.temp$id <- factor(df.grp.temp$id, levels = df.grp.temp$id)
  
  ggplot(df.grp.temp, aes(x = id, y = avg, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("MADRS score across condition group") +
    xlab("id") + ylab(" MADRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) +
    theme_bw()
  
  t1 <- df.grp1[order(df.grp1$avg),]
  t1 <- df.grp1[order(df.grp1$avg),]
  
  ggplot(df.grp.temp, aes(x = id, y = afftype, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("affinity type across condition group") +
    xlab("id") + ylab(" affinity type") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp, aes(x = id, y = age, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green")) +
    ggtitle("Age distribution across groups") +
    xlab("id") + ylab(" age") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) +
    theme_bw()
 
  ggplot(df.grp, aes(x = id, y = gender, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green")) +
    ggtitle("Gender distribution across groups") +
    xlab("id") + ylab("gender") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
 
  ggplot(df.grp, aes(x = id, y = work  , fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green")) +
    ggtitle("Days distribution across groups") +
    xlab("id") + ylab("days") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
     
  ggplot(df.grp, aes(x = id, y = afftype, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green")) +
    ggtitle("mean activity across groups") +
    xlab("id") + ylab(" mean activity") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  
  df.grp1 <- subset(df.chars, )
  df.grp2 <- subset(df.chars, score > 0 & id < 24)
  df.grp3 <- subset(df.chars, score == 0 & id > 23)
  df.grp4 <- subset(df.chars, score > 0 & id > 23)
  
  df.grp <- rbind(df.grp1, df.grp2, df.grp3, df.grp4)
  df.grp$id <- factor(df.grp$id, levels = df.grp$id)
  
    ggplot(df.grp1, aes(x=id, y=gender, fill = gender)) +
    geom_bar(stat="identity")+
    #geom_text(aes(label=avg), vjust=1.6, color="white", size=5)+
    ggtitle("degree of each node in community 6") +
    xlab("id") + ylab("degree") +
    scale_fill_manual(values=c(  "red",
                                 "darkblue",
                                 "red",
                                 "darkblue",
                                 "red",
                                 "darkblue",
                                 "red",
                                 "darkblue",
                                 "darkblue",
                                 "red",
                                 "darkblue",
                                 "green",
                                 "black"
                                 ))
    
    ggplot(data=df.grp1, aes(x=id, y= avg))+
      geom_bar(stat="identity")+
      scale_fill_manual(values=c("#9933FF",
                                 "#33FFFF",
                                 "red"
                                 ))
    
    #+
    #theme(axis.title = element_text(size = 15)) +
    #theme(axis.text = element_text(size = 15))  +
    #theme(plot.title = element_text(size =15)) 
    #+
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
     #     panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
    #, fill="springgreen3"
  
  
  ########## plot graph according to groups
  

  for (i in 1:23) {
    

    
    if ((node.score$score[i] == 0 ) && (i != 2) && (i != 14) && (i != 18))
    {
      V(network)$color[i] <- "Red"
      V(network)$label.color[i] <- "white"
    }
    else if (( i == 2) || ( i == 14) || (i == 18))
    {
      V(network)$color[i] <- "blue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    else
    {
      V(network)$color[i] <- "purple"
      V(network)$label.color[i] <- "white"
    }
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  #V(network)$color <- "orange"
  #V(network)$label <- "" 
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {


    if((node.score$score[i] == 0 ) && (i != 44))
    {
      V(network)$color[i] <- "green"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }
    else if( i == 44)
    {
      V(network)$color[i] <- "blue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
    }
    else
    {
      V(network)$color[i] <- "orange"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
      
    }
  }
  
  plot(network, vertex.label.font = 2)
    plot(network, layout_with_kk(network), vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition Group1 (score = 0)", "Condition Group2 (score > 0)",
                       "Control Group1 (score = 0)","Control Group2 (score > 0)","Singleton Group"), 
         pch=c(19,19,15,15,15), title="Groups by Score (inter/intra)",
         col=c( "red", "purple", "green", "orange", "blue"), 
         pt.bg=c("red", "purple", "green", "orange", "blue"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
    #######
    
  
  }


##################################################################
# Plot_graph_super
##################################################################

Plot_graph_super <- function(network)
{
  
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "square"
    
    if (0) #(( i == 2) || (i == 14) || (i == 18))
    {
      V(network)$color[i] <- "blue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
    }
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if ( i == 44)
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
      
      #V(network)$color[i] <- "blue"
      #V(network)$label.color[i] <- "white"
      #V(network)$shape[i] <- "square"
    }
  }
  
  plot(network, layout=layout_with_kk(network), vertex.label.font = 2)
  plot(network, layout=layout_nicely, vertex.label.font = 2)


}

##################################################################
# Plot graph before clustering
##################################################################

Plot_graph_before_cluster <- function(network)
{
  
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if(( i == 2) || ( i == 14) || ( i == 18))
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 44)
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  }
  
  plot(network, layout=layout_nicely, vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition group", "Control group"), 
         pch=c(19,15), title="Correlation network graph",
         col=c( "red", "green"), 
         pt.bg=c("red", "green"), 
         pt.cex=1.6, cex=.8, bty="n")
  
}


##################################################################
# Plot graph after clustering
##################################################################

Plot_graph_after_cluster_M1 <- function(network)
{
  
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if(( i == 2) || ( i == 14) || ( i == 18))
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 44)
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  }
  
  plot(network, layout = layout_with_kk(network), vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition group", "Control group"), 
         pch=c(19,15), title="Correlation network graph",
         col=c( "red", "green"), 
         pt.bg=c("red", "green"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  exp = 2; infl = 2 ; features = "cluster"
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "blue"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if ( i == 2)
    {
      V(network)$color[i] <- "purple"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }

    if ( i == 14)
    {
      V(network)$color[i] <- "gray50"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    if ( i == 18)
    {
      V(network)$color[i] <- "pink"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }

  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "orange"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 44)
    {
      #V(network)$color[i] <- "skyblue"
      #V(network)$label.color[i] <- "black"
      #V(network)$shape[i] <- "square"
      
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
      
    }
    
    
    
  }
  
  plot(network, layout = layout_with_kk(network), vertex.label.font = 2)

  plot(network, layout = layout_nicely, vertex.label.font = 2)
  
  legend('topleft',  c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6"), 
         pch=c(19,15,19,19,15,19), title="Communities",
         col=c( "blue", "orange", "purple", "gray50", "skyblue", "pink"), 
         pt.bg=c( "red", "green", "blue", "gray50", "gold", "pink"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  #confusion matrix with known groups
  km_plot$group <- NA
  km_plot$group[1:23] <- 1
  km_plot$group[24:55] <- 2
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "2")
  
  km_plot$group[1:23] <- 2
  km_plot$group[24:55] <- 1
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "1")
  
}


Plot_graph_after_cluster_M2 <- function(network)
{
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if ((i == 14))
    {
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if((i == 25))
    {
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
    }
  }
  
  
  plot(network, layout = layout_with_kk(network), vertex.label.font = 2)
  plot(network, layout = layout_nicely, vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition group", "Control group"), 
         pch=c(19,15), title="Correlation network graph",
         col=c( "red", "green"), 
         pt.bg=c("red", "green"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  exp = 3; infl = 3 ; features = "cluster"
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "blue"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if ( i == 14)
    {
      V(network)$color[i] <- "purple"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
    }
    
  
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "orange"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 25)
    {
      V(network)$color[i] <- "skyblue"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }

  }
  
  plot(network,  vertex.label.font = 2)
  
  legend('topleft',  c("cluster 1", "cluster 2","cluster 3","cluster 4"), 
         pch=c(19,15,19,15), title="Communities",
         col=c( "blue", "orange", "purple", "skyblue"), 
         pt.bg=c( "red", "green", "blue", "gold"), 
         pt.cex=1.6, cex=.8, bty="n")
  

  #confusion matrix with known groups
  km_plot$group <- NA
  km_plot$group[1:23] <- 1
  km_plot$group[24:55] <- 2
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "2")
  
  km_plot$group[1:23] <- 2
  km_plot$group[24:55] <- 1
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "1")  
  
}


Plot_graph_after_cluster_M3 <- function(network)
{
  
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if((i == 2) || (i == 14) || (i == 15) || (i == 17) || (i == 18) || (i == 20) )
    {
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
    }
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    
  }
  
  plot(network, vertex.label.font = 2)
  plot(network, layout = layout_with_kk(network), vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition group", "Control group"), 
         pch=c(19,15), title="Correlation network graph",
         col=c( "red", "green"), 
         pt.bg=c("red", "green"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  exp = 2; infl = 2; features = "cluster"
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "blue"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "circle"
    
    if ( i == 2)
    {
      V(network)$color[i] <- "purple"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    if ( i == 14)
    {
      V(network)$color[i] <- "gray50"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    if ( i == 15)
    {
      V(network)$color[i] <- "skyblue"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    if (( i == 17) )
    {
      V(network)$color[i] <- "pink"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    if ( i == 18)
    {
      V(network)$color[i] <- "cyan"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    if ( i == 20)
    {
      V(network)$color[i] <- "limegreen"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    V(network)$color[i] <- "orange"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
  }
  
  #plot(network,  layout = layout_with_kk(network), vertex.label.font = 2)
  plot(network,  vertex.label.font = 2)
  
  legend('topleft',  c("cluster 1", "cluster 2","cluster 3","cluster 4","cluster 5","cluster 6", "cluster 7", "cluster 8"), 
         pch=c(19,15,19,19,19,19,19,19), title="Communities",
         col=c( "blue", "orange", "skyblue", "gray50", "purple", "limegreen", "cyan", "pink"), 
         pt.bg=c( "blue", "orange", "skyblue", "gray50", "purple", "limegreen", "cyan", "pink"), 
         pt.cex=1.6, cex=.8, bty="n")
  

  
  #confusion matrix with known groups
  
  km_plot <- NA
  km_plot$id <- 1:55
  km_plot$cluster <- mcl.res$Cluster
  km_plot <- as.data.frame(km_plot)
  km_plot <- km_plot[,-1]
  
  cmat <- NA
  cmat$group <- NA
  cmat$group[1:23] <- 1
  cmat$group[24:55] <- 2
  cmat$cluster <- NA
  cmat$cluster <- as.factor(mcl.res$Cluster)
  cmat$group <- as.factor(cmat$group)
  confusionMatrix(cmat$group$cluster, cmat$group$group, positive = "2")
  
  km_plot$group[1:23] <- 2
  km_plot$group[24:55] <- 1
  km_plot$cluster <- as.factor(mcl.res)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "1")  
  
}


##################################################################
# plot cliques
##################################################################

plot_cliques_control <- function(network)
{

    min.cliques <-   cliques(network, min=11)

        clique.network <- largest_cliques(network)
    for (i in 1:length(clique.network)) {
      temp.network <- subgraph(network, clique.network[[1]])
      for (j in 1:length(V(temp.network))) {
        
        V(temp.network)$color[j] <- "green"
        V(temp.network)$label.color[j] <- "black"
        V(temp.network)$shape[j] <- "square"
        
      }
      plot(temp.network, layout=layout_nicely, vertex.label.font = 2)
    }  

}

##################################################################
# mcl clustering method applied on the correlation matrix
##################################################################

mcl_cluster <- function(cor_matrix, network, exp, infl, features)
{

    #coloring vertices
    #http://rstudio-pubs-static.s3.amazonaws.com/5014_4e3001382f7442629c0760f373cdadd4.html
  
    exp = 3; infl = 3 ; features = "cluster"
    exp = 2; infl = 2 ; features = "cluster"
    
    mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
    print(mcl.res$Cluster[24:55])
    
    sg <- list()
    tot_edges <- 0
    for (i in 1:mcl.res$K) {
        nodes <- c()
        for (j in 1:55) {
        if(mcl.res$Cluster[j] == i)
        nodes <- c(nodes, V(network)$name[j])
        }
      sg[[i]] <- induced.subgraph(network, nodes)
      #print(length(nodes))
      #print(gsize(sg[[i]]))
      tot_edges <- tot_edges + gsize(sg[[i]])
      #Density
      #print (edge_density(sg[[i]]))
              
    }
   
    print("Modularity")
    modularity(network, mcl.res$Cluster)
    print("Coverage")
    tot_edges/gsize(network)

    
    
    #color the nodes according to their cluster number
    vertex.color <- vector()
    vertex.color[1:55] <- "skyblue"
    pal <- brewer.pal(mcl.res$K, "Set3")
    for (i in 1:55) {
    
      #V(network)$color <- pal[mcl.res$Cluster]
        if(1)
        {
        if(mcl.res$Cluster[i] == 1)
          vertex.color[i] <- "skyblue"
        else if(mcl.res$Cluster[i] == 2)
          vertex.color[i] <- "blue"
        else if(mcl.res$Cluster[i] == 3)
          vertex.color[i] <- "orange"
        else if(mcl.res$Cluster[i] == 4)
          vertex.color[i] <- "purple"
        else if(mcl.res$Cluster[i] == 5)
          vertex.color[i] <- "red"
        else if(mcl.res$Cluster[i] == 6)
          vertex.color[i] <- "grey"
        else if(mcl.res$Cluster[i] == 7)
          vertex.color[i] <- "green"
        }
    }
  
    plot(network, vertex.size=8, 
       vertex.color = vertex.color,
       cex=.2,
       layout=igraph::layout.fruchterman.reingold(network, niter=10000),
       main=paste("MCL clustering algorithm \n", "features -",features," \n number of clusters =",mcl.res$K ))
  
    legend('topleft',  c("cluster1", "cluster2","cluster3","cluster4","cluster5","cluster6"), 
           pch=21, title="clusters",
           col=c( "skyblue", "pink", "orange", "lightgreen","red" ,"grey"), 
           pt.bg=c( "skyblue","pink", "orange", "lightgreen","red" ,"grey"), 
           pt.cex=2, cex=.8, bty="n", ncol=1)
    
    

    
    return(mcl.res)
    
}

##################################################################
# k-means clustering method applied on the correlation graph
##################################################################

network_kmeans_clustering_with_schz <- function(network)
{
  # k-means clustering
  A = get.adjacency(network, sparse=FALSE)
  
  k <- 3
  km <- kmeans(A, centers = k, nstart = 1)
  km$tot.withinss
  km_plot <- NA
  km_plot$id <- 1:77
  km_plot$cluster <- km$cluster
  km_plot <- as.data.frame(km_plot)
  km_plot <- km_plot[,-1]
  ggplot(km_plot, aes(x = 1:77, y = cluster, color = cluster)) + geom_point() +
    geom_text(aes(label=id),hjust=0, vjust=0)
  
  #confusion matrix with known groups
  km_plot$group <- NA
  km_plot$group[1:23] <- 1
  km_plot$group[24:55] <- 2
  km_plot$group[56:77] <- 3
  
  km_plot$cluster <- as.factor(km_plot$cluster)
  km_plot$group <- as.factor(km_plot$group)
  confusionMatrix(km_plot$cluster, km_plot$group, positive = "2")
}

