#############################################################################
# File name: model_sch.R
# Author: Rama Thelagahoti
# Description: model building with sczh
############################################################################

rownames_schz <-  c("p1", "p2", "p3", "p4", "p5", "p6", "p7" , "p8" , "p9", "p10",
                  "p11", "p12", "p13", "p14", "p15", "p16", "p17" , "p18" , "p19", "p20" ,
                  "p21", "p22", "p23", "p24", "p25", "p26", "p27" , "p28" , "p29", "p30" ,
                  "p31", "p32", "p33", "p34", "p35", "p36", "p37" , "p38" , "p39", "p40" ,
                  "p41", "p42", "p43", "p44", "p45", "p46", "p47" , "p48" , "p49", "p50" ,
                  "p51", "p52", "p53", "p54", "p55", "p56", "p57",  "p58",  "p59" ,"p60",
                  "p61", "p62", "p63", "p64", "p65", "p66", "p67",  "p68",  "p69" ,"p70",
                  "p71", "p72", "p73", "p74", "p75", "p76", "p77"
)
colnames_schz = rownames_schz

############################################################################
# Function: model_hourly_all_with_schz
# Description: model building with scz
############################################################################
model_hourly_all_with_schz <- function(deprs.features, cont.features,schz.features)
{
  
  #https://stackoverflow.com/questions/65271523/r-calculate-clustering-coefficient-of-each-node
    features <- rbind(deprs.features, cont.features,schz.features)
  
  #tmean <- aggregate( features, by = list(features[, 1]), FUN = mean)
  #write.csv(tmean,"tmean.csv")
  
  preproc <- preProcess(deprs.features[,c(4:54)], method=c("range"))
  deprs.features.st <- predict(preproc, deprs.features[,c(4:54)])
  #str(cond_stand)
  
  preproc <- preProcess(cont.features[,c(4:54)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:54)])
  #str(cont_stand)
  
  preproc <- preProcess(schz.features[,c(4:54)], method=c("range"))
  schz.features.st <- predict(preproc, schz.features[,c(4:54)])
  #str(cont_stand)
  
  features.st <- rbind(deprs.features.st, cont.features.st,schz.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:51) {
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
  
  cor_matrix <- matrix(0, nrow = length(rownames_schz), ncol = length(colnames_schz),  
                       dimnames = list(rownames_schz, colnames_schz))
  for (i in 1:length(rownames_schz)) {
    for (j in 1:length(colnames_schz)) {
      
      if(( cormat[i,j] >= 0.7 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  
  #https://stackoverflow.com/questions/32857024/increasing-the-distance-between-igraph-nodes
  node.size= c(10,10,10)
  plot(network, vertex.size= node.size*0.5)

  plot(network, rescale = FALSE, ylim=c(1,4),xlim=c(-17,24), asp = 0)
  
  #https://stackoverflow.com/questions/11272349/igraph-axes-xlim-ylim-plot-incorrectly
  plot(network,  layout=layout_nicely, vertex.size = 5, vertex.label.cex = 0.8)

  
  
  
  #Day1
  
  d1.depr.features <- deprs.features %>% filter( day == 1 | day == 2 | day == 3 )
  d1.cont.features <- cont.features %>% filter( day == 1 | day == 2 | day == 3)
  d1.schz.features <- schz.features %>% filter( day == 1 | day == 2 | day == 3)
  
  preproc <- preProcess(d1.depr.features[,c(4:54)], method=c("range"))
  d1.depr.features.st <- predict(preproc, d1.depr.features[,c(4:54)])
  #str(cond_stand)
  
  preproc <- preProcess(d1.cont.features[,c(4:54)], method=c("range"))
  d1.cont.features.st <- predict(preproc, d1.cont.features[,c(4:54)])
  #str(cont_stand)
  
  preproc <- preProcess(d1.schz.features[,c(4:54)], method=c("range"))
  d1.schz.features.st <- predict(preproc, d1.schz.features[,c(4:54)])
  #str(cont_stand)
  
  features.st <- rbind(d1.depr.features.st, d1.cont.features.st,d1.schz.features.st)
  #str(stand_sep)
  
  #outlier treatment
  boxplot(features.st)
  for (i in 1:51) {
    features.st[ , i] <- outlier_norm(features.st[ ,i])  
  }
  boxplot(features.st)
  
  # pearson correlation
  features.matrix <- as.data.frame(features.st)
  
  cormat <- cor(t(features.matrix), method = "pearson")
  corrplot::corrplot(cormat, main="Correlation Plot for Numerical Variables", method="number")
  hist(cormat)
  
  cor_matrix <- matrix(0, nrow = length(rownames_schz), ncol = length(colnames_schz),  
                       dimnames = list(rownames_schz, colnames_schz))
  for (i in 1:length(rownames_schz)) {
    for (j in 1:length(colnames_schz)) {
      
      if(( cormat[i,j] >= 0.55 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot(network )
  
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
    
    if(0)#( i == 2) || ( i == 14) || ( i == 18))
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
    
    if(0)#(i == 44))
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  }
  
  for (i in 55:77) {
    
    V(network)$color[i] <- "skyblue"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if(0 ) #(i == 57))
    {
      V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      V(network)$size[i] <- 0 
    }
    
    
  
  }
  plot(network, layout=layout_nicely, vertex.label.font = 2)
  
  legend('bottomleft',  c("Depressed", "Healthy", "schizophrenia"),
         pch=c(19,15,15), title="Correlation network graph",
         col=c( "red", "green", "skyblue"), 
         pt.bg=c("red", "green", "skyblue"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  exp = 3; infl = 2 ; features = "cluster"
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  
  for (i in 1:23) {
    
    V(network)$color[i] <- "red"
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
    
    if ( i == 18)
    {
      V(network)$color[i] <- "orange"
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
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 44)
    {
      V(network)$color[i] <- "gold"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }
    
  }
  
  for (i in 55:77) {
    
    V(network)$color[i] <- "skyblue"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    if( i == 57)
    {
      V(network)$color[i] <- "pink"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }
    
  }
  plot(network,  layout=layout_nicely, vertex.size = 6, vertex.label.cex = 0.8)
  
  plot(network,  layout=layout_with_kk(network), vertex.size = 6, vertex.label.cex = 0.8)
  
  #plot(network,  layout = layout_with_kk(network), vertex.label.font = 2)
  plot(network,  vertex.label.font = 2)
  
  #plot(network,layout=layout_nicely,rescale=F,axes=TRUE,ylim=c(-10,10),xlim=c(-5,15), asp = 0, vertex.size = 5, vertex.label.cex = 0.8)
  
  legend('topleft',  c("cluster 1", "cluster 2","cluster 3","cluster 4","cluster 5","cluster 6", "cluster 7", "cluster 8"), 
         pch=c(19,15,19,19,19,19,19,19), title="Communities",
         col=c( "red", "green", "skyblue", "gray50", "purple", "gold", "orange", "pink"), 
         pt.bg=c( "red", "green", "skyblue", "gray50", "purple", "gold", "orange", "pink"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  
}

node_analysis()
{
  
  exp = 3; infl = 2 ; 
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  #find degree of each node and stote it in a vector
  deg <- igraph::degree(network, mode="all")
  t <- as.data.frame(deg)
  degree <- t$deg
  
  
  #find intra-inter cluster edges
  
  nrow = 77
  ncol = 15
  node.score <- data.frame(matrix(ncol = ncol, nrow = nrow))
  cols <- c("id","clst","deg", "intra", "inter", "score",
            "btns", "clns", "eign", "auth", "hub", "coef","cointra","cointer","coscr")
  colnames(node.score) <- cols
  node.score$id <- 1:nrow
  node.score$clst <- mcl.res$Cluster
  node.score[is.na(node.score)] = 0
  node.score$deg <- degree
  
  net.matrix = get.adjacency(network, sparse=FALSE)
  # since its a 55x55 matrix , use 2 loop variable i & j
  for (i in 1:nrow) {
    for (j in 1:nrow) {
      # if edge present & not a loop
      if((net.matrix[i,j] == 1) && (i != j))
      {
        # same cluster then intra else inter edge
        if(node.score$clst[i] == node.score$clst[j])
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
  
  node.score$btns <- round(betweenness(cor_matrix),3)
  node.score$clns <- round(closeness(cor_matrix),3)
  node.score$eign <- round(evcent(cor_matrix),3)
  node.score$auth <- round(authority.score(network)$vector,3)
  node.score$hub <- round(hub.score(network)$vector,3)
  node.score$coef <- round(transitivity(network, type = "local"), 3)
  #node.score$coef[1:32] <- round(transitivity(network, vids = controls, type = "local"), 3)
  #node.score$coef[33:54] <- round(transitivity(network, vids = schzs, type = "local"), 3)
  #node.score$ccoef [is.na(node.score$ccoef)] <- 0
  
  citmat <- cocitation(network)
  #intra
  citmat.deprs <- citmat[1:23,1:23]
  citmat.deprs.sums <- rowSums(citmat.deprs)
  citmat.deprs.sums <- as.data.frame(citmat.deprs.sums)
  citmat.cont <- citmat[24:55,24:55]
  citmat.cont.sums <- rowSums(citmat.cont)
  citmat.cont.sums <- as.data.frame(citmat.cont.sums)
  citmat.scz <- citmat[56:77,56:77]
  citmat.scz.sums <- rowSums(citmat.scz)
  citmat.scz.sums <- as.data.frame(citmat.scz.sums)
  node.score$cointra <- append(citmat.deprs.sums$citmat.deprs.sums,
                               append(citmat.cont.sums$citmat.cont.sums,
                               citmat.scz.sums$citmat.scz.sums))
  #inter
  citmat.deprs <- citmat[1:23,24:77]
  citmat.deprs.sums <- rowSums(citmat.deprs)
  citmat.deprs.sums <- as.data.frame(citmat.deprs.sums)
  citmat.cont <- citmat[24:55,c(1:23,56:77)]
  citmat.cont.sums <- rowSums(citmat.cont)
  citmat.cont.sums <- as.data.frame(citmat.cont.sums)
  citmat.scz <- citmat[56:77,1:55]
  citmat.scz.sums <- rowSums(citmat.scz)
  citmat.scz.sums <- as.data.frame(citmat.scz.sums)
  node.score$cointer <- append(citmat.deprs.sums$citmat.deprs.sums,
                               append(citmat.cont.sums$citmat.cont.sums,
                               citmat.scz.sums$citmat.scz.sums))
  node.score$coscr <- (node.score$cointer/node.score$cointra)
  
  df.chars <-  cbind(node.score, scores.merged)
  
  # get mean 
  meandf <- rbind( deprs.features,cont.features, schz.features)
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[37] <- "mean"
  
  #write.csv(df.chars, "M1.csv")
  
  #all correlation
  df.chars.cor <- df.chars[,c(3:7,9:15,37)]
  df.chars.cor <- df.chars.cor %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor <- cor(df.chars.cor, method = "pearson")
  corrplot::corrplot(df.cor,  main= "Control & Schz", type = "lower", method="number")
  
  #deprs correlation
  df.chars.cor1 <- df.chars[1:23,c(3:7,9:15,37)]
  df.chars.cor1 <- df.chars.cor1 %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor1 <- cor(df.chars.cor1, method = "pearson")
  corrplot::corrplot(df.cor1, main= "Control",  type = "lower",method="number")
  
  #contr correlation
  df.chars.cor2 <- df.chars[24:55,c(3:7,9:15,37)]
  df.chars.cor2 <- df.chars.cor2 %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor2 <- cor(df.chars.cor2, method = "pearson")
  corrplot::corrplot(df.cor2, main= "Schz",  type = "lower",method="number")
  
  #schz correlation
  df.chars.cor2 <- df.chars[56:77,c(3:7,9:15,37)]
  df.chars.cor2 <- df.chars.cor2 %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor2 <- cor(df.chars.cor2, method = "pearson")
  corrplot::corrplot(df.cor2, main= "Schz",  type = "lower",method="number")
  
  ######
  
}

plot_node_categories <- function()
{
  
  
  for (i in 1:23) {
    
    if(node.score$score[i] == 0)
    {
      V(network)$color[i] <- "red"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
    }
    else
    {
      V(network)$color[i] <- "purple"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
    }
    
    if ( i == 2)
    {
      V(network)$color[i] <- "gray50"
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
      V(network)$color[i] <- "gray50"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      
    }
    
    
  }
  
  V(network)$size <- 7
  V(network)$frame.color <-  adjustcolor("white", alpha.f = .0) #"white"
  V(network)$label.cex <- 0.7	 
  E(network)$arrow.mode <- 0
  
  for (i in 24:55) {
    
    if(node.score$score[i] == 0)
    {  
      V(network)$color[i] <- "green"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "circle"
    }
    else
    {
      V(network)$color[i] <- "orange"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "circle"
      
    }
    if( i == 44)
    {
      V(network)$color[i] <- "gray50"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
    }
    
  }
  
  for (i in 56:77) {
    
    if(node.score$score[i] == 0)
    {  
      V(network)$color[i] <- "royalblue1"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
    }
    else
    {
      V(network)$color[i] <- "skyblue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
      
    }
    if( i == 57)
    {
      V(network)$color[i] <- "gray50"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "circle"
    }
    
  }
  plot(network,  layout=layout_nicely, vertex.size = 5, vertex.label.cex = 0.6)
  
  plot(network,  layout=layout_with_kk(network), vertex.size = 6, vertex.label.cex = 0.8)
  
  #plot(network,  layout = layout_with_kk(network), vertex.label.font = 2)
  plot(network,  vertex.label.font = 2)
  
  #plot(network,layout=layout_nicely,rescale=F,axes=TRUE,ylim=c(-10,10),xlim=c(-5,15), asp = 0, vertex.size = 5, vertex.label.cex = 0.8)
  
  legend('topleft',  c("Deprssed Community", "Deprssed Boundary","Healthy Community","Healthy Boundary",
                       "Schizophrenia Community","Schizophrenia Boundary",  "Singleton cluster"), 
         pch=c(19,19,15,15,15,15,19), title="Severity Estimation",
         col=c( "red", "purple", "green", "orange", "royalblue1", "skyblue","gray50"), 
         pt.bg=c( "red", "purple", "green", "orange", "royalblue1", "skyblue", "gray50"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
}


plot_categories_analysis <- function()
{
  
  df.chars$grp <- NA
  for (i in 1:23) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- "1_D_C"
    else
      df.chars$grp[i] <- "2_D_B"
  }
  
  for (i in 24:55) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- "3_C_C"
    else
      df.chars$grp[i] <- "4_C_B"
  }
  
  for (i in 56:77) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- "5_S_C"
    else
      df.chars$grp[i] <- "6_S_B"
  }
  df.chars$grp <-as.factor(df.chars$grp)
  
  df.grp1 <- subset(df.chars, score == 0 & id < 24 & id !=2 & id != 14 & id != 18)
  df.grp2 <- subset(df.chars, score > 0 & id < 24 & id !=2 & id != 14 & id != 18)
  df.grp3 <- subset(df.chars, score == 0 & id > 23 & id < 56 & id != 44)
  df.grp4 <- subset(df.chars, score > 0 & id > 23 & id < 56 & id != 44)
  df.grp5 <- subset(df.chars, score == 0 & id > 55 & id != 57)
  df.grp6 <- subset(df.chars, score > 0 & id > 55 & id != 57)
  
  df.grp <- rbind(df.grp1, df.grp2, df.grp3, df.grp4, df.grp5, df.grp6)
  df.grp$id <- factor(df.grp$id, levels = df.grp$id)

  ggplot(df.grp, aes(x = id, y = mean, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "green", "pink","skyblue","maroon")) +
    ggtitle("mean activity across groups") +
    xlab("id") + ylab(" mean activity") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  #MADRS
  df.grp.temp <- rbind(df.grp1, df.grp2)
  df.grp.temp$id <- factor(df.grp.temp$id, levels = df.grp.temp$id)
  
  ggplot(df.grp.temp, aes(x = id, y = madrs1, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("MADRS score across condition group") +
    xlab("id") + ylab(" MADRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) +
    theme_minimal()

  ggplot(df.grp.temp, aes(x = id, y = madrs2, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("MADRS score across condition group") +
    xlab("id") + ylab(" MADRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp.temp, aes(x = id, y = afftype, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("affinity type across condition group") +
    xlab("id") + ylab(" affinity type") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp, aes(x = id, y = gender, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "green", "pink","skyblue","maroon")) +
    ggtitle("Age distribution across groups") +
    xlab("id") + ylab(" age") +
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
  #BPRS
  df.grp.temp2 <- rbind(df.grp5, df.grp6)
  df.grp.temp2$id <- factor(df.grp.temp2$id, levels = df.grp.temp2$id)
  df.grp.temp22 <- df.grp.temp2[-c(4,13,14,20,21) ,]
  ggplot(df.grp.temp22, aes(x = id, y = bprs, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("royalblue1", "skyblue")) +
    ggtitle("BPRS score across schz group") +
    xlab("id") + ylab(" BPRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) +
    theme_minimal()
  
  ggplot(df.grp.temp2, aes(x = id, y = schtype, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("skyblue", "maroon")) +
    ggtitle("schztype  across schz group") +
    xlab("id") + ylab(" BPRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp.temp2, aes(x = id, y = migraine, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("skyblue", "maroon")) +
    ggtitle("migrane across schz group") +
    xlab("id") + ylab(" BPRS score") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 

  ggplot(df.grp.temp2, aes(x = id, y = cloz, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("skyblue", "maroon")) +
    ggtitle("cloz across schz group") +
    xlab("id") + ylab(" cloz type") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 

  ggplot(df.grp.temp2, aes(x = id, y = trad, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("skyblue", "maroon")) +
    ggtitle("trad across schz group") +
    xlab("id") + ylab(" trad type") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 

  ggplot(df.grp.temp2, aes(x = id, y = moodst, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("skyblue", "maroon")) +
    ggtitle("moodst across schz group") +
    xlab("id") + ylab(" moodst type") +
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
  
  
  
}
node_analysis_old <- function(cor_matrix, network, exp, infl, features)
{
  
  
  exp = 3; infl = 2 ; features = "cluster"
  
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
  
  for (i in 1:length(degree))
  {
    #print(E(tempnet)[V(tempnet)[name== paste("p",i,sep = "") ] %--% V(tempnet)])
  }
  
  #find intra-inter cluster edges
  
  node.score <- data.frame(matrix(ncol = 5, nrow = 77))
  cols <- c("id","cluster", "intra", "inter", "score")
  colnames(node.score) <- cols
  node.score$id <- 1:77
  node.score$cluster <- mcl.res$Cluster
  node.score[is.na(node.score)] = 0
  
  
  net.matrix = get.adjacency(network, sparse=FALSE)
  # since its a 55x55 matrix , use 2 loop variable i & j
  for (i in 1:77) {
    
    for (j in 1:77) {
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
  
  df.chars <-  cbind(node.score, rbind(df.scores,df.schz.info))
  df.chars$avg <- colMeans(rbind(df.chars$madrs1, df.chars$madrs2), na.rm=TRUE)
  write.csv(node.score, "nodescore.csv")
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
  colnames(df.chars)[22] <- "mean"
  
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
    theme(plot.title = element_text(size =15)) 
  
  
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
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp.temp, aes(x = id, y = afftype, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple")) +
    ggtitle("affinity type across condition group") +
    xlab("id") + ylab(" affinity type") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
  ggplot(df.grp, aes(x = id, y = gender, fill = grp)) +
    geom_bar(stat="identity", width = 0.5) +
    scale_fill_manual(values=c("red", "purple", "orange", "green")) +
    ggtitle("Age distribution across groups") +
    xlab("id") + ylab(" age") +
    guides(fill=guide_legend(title="Group")) +
    theme(axis.title = element_text(size = 15)) + 
    theme(axis.text = element_text(size = 10))  +
    theme(plot.title = element_text(size =15)) 
  
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
  #plot(network, layout_with_kk(network), vertex.label.font = 2)
  
  legend('bottomleft',  c("Condition Group1 (score = 0)", "Condition Group2 (score > 0)",
                          "Control Group1 (score = 0)","Control Group2 (score > 0)","Singleton Group"), 
         pch=c(19,19,15,15,15), title="Groups by Score (inter/intra)",
         col=c( "red", "purple", "green", "orange", "blue"), 
         pt.bg=c("red", "purple", "green", "orange", "blue"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  #######
  
  
}


