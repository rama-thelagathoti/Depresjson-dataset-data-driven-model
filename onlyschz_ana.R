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

rownames.54 <-  c("p24", "p25", "p26", "p27" , "p28" , "p29", "p30" ,
                  "p31", "p32", "p33", "p34", "p35", "p36", "p37" , "p38" , "p39", "p40" ,
                  "p41", "p42", "p43", "p44", "p45", "p46", "p47" , "p48" , "p49", "p50" ,
                  "p51", "p52", "p53", "p54", "p55", "p56", "p57",  "p58",  "p59" ,"p60",
                  "p61", "p62", "p63", "p64", "p65", "p66", "p67",  "p68",  "p69" ,"p70",
                  "p71", "p72", "p73", "p74", "p75", "p76", "p77"
)
colnames.54 = rownames.54

controls <- c("p24", "p25", "p26", "p27" , "p28" , "p29", "p30" ,
              "p31", "p32", "p33", "p34", "p35", "p36", "p37" , "p38" , "p39", "p40" ,
              "p41", "p42", "p43", "p44", "p45", "p46", "p47" , "p48" , "p49", "p50" ,
              "p51", "p52", "p53", "p54", "p55")
schzs <- c("p56", "p57",  "p58",  "p59" ,"p60",
           "p61", "p62", "p63", "p64", "p65", "p66", "p67",  "p68",  "p69" ,"p70",
           "p71", "p72", "p73", "p74", "p75", "p76", "p77")

############################################################################
# Function: Model_hourly_final
# Description: model building with only hourly mean and hourly sd
############################################################################
Model_schz_control_hourly <- function(schz.features, cont.features)
{
  
  features <- rbind(cont.features, schz.features )
  
  preproc <- preProcess(schz.features[,c(4:51)], method=c("range"))
  schz.features.st <- predict(preproc, schz.features[,c(4:51)])

  preproc <- preProcess(cont.features[,c(4:51)], method=c("range"))
  cont.features.st <- predict(preproc, cont.features[,c(4:51)])

  features.st <- rbind(cont.features.st, schz.features.st)

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
  rownames(features.matrix.matrix) <- rownames.54
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
  
  cor_matrix <- matrix(0, nrow = length(rownames.54), ncol = length(colnames.54),  dimnames = list(rownames.54, colnames.54))
  for (i in 1:length(rownames.54)) {
    for (j in 1:length(colnames.54)) {
      
      if(( cormat[i,j] >= 0.65 ) && ( i != j))
      {
        cor_matrix[i,j] <- 1
      }
    }
  }  
  #write.csv(cormat, "cor_matrix_n.csv")
  #write_graph(network, "edgelist_2.csv", "edgelist")
  
  
  
  network <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected")
  plot(network)

  exp = 3; infl = 2 ; features = "cluster"
  
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  
}

plot_graph()
{
  
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 1:32) {
    
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "square"
    
    if( i == 21)
    {
      V(network)$color[i] <- "skyblue"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }
    
  }
  
  for (i in 33:547) {
    
    V(network)$color[i] <- "red"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "square"
    
    if( i == 34)
    {
      V(network)$color[i] <- "pink"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
    }
    
  }
  
  #plot(network,  layout = layout_with_kk(network), vertex.label.font = 2)
  plot(network,  vertex.label.font = 2)
  
  legend('topleft',  c("cluster 1", "cluster 2","cluster 3","cluster 4"), 
         pch=c(15,15,15,15), title="Communities",
         col=c( "red", "green", "skyblue", "pink"), 
         pt.bg=c( "red", "green", "skyblue", "pink"), 
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
  
  nrow = 54
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
  citmat.cont <- citmat[1:32,1:32]
  citmat.cont.sums <- rowSums(citmat.cont)
  citmat.cont.sums <- as.data.frame(citmat.cont.sums)
  citmat.scz <- citmat[33:54,33:54]
  citmat.scz.sums <- rowSums(citmat.scz)
  citmat.scz.sums <- as.data.frame(citmat.scz.sums)
  node.score$cointra <- append((citmat.cont.sums$citmat.cont.sums),(citmat.scz.sums$citmat.scz.sums))
  #inter
  citmat.cont <- citmat[1:32,33:54]
  citmat.cont.sums <- rowSums(citmat.cont)
  citmat.cont.sums <- as.data.frame(citmat.cont.sums)
  citmat.scz <- citmat[33:54,1:32]
  citmat.scz.sums <- rowSums(citmat.scz)
  citmat.scz.sums <- as.data.frame(citmat.scz.sums)
  node.score$cointer <- append((citmat.cont.sums$citmat.cont.sums),(citmat.scz.sums$citmat.scz.sums))
  node.score$coscr <- (node.score$cointer/node.score$cointra)
  
  df.chars <-  cbind(node.score, scores.merged[24:77,])

  # get mean 
  meandf <- rbind(cont.features,schz.features )
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[37] <- "mean"
  
#  write.csv(df.chars, "dfchars_cont_schz_6_1.csv")
  
  #all correlation
  df.chars.cor <- df.chars[,c(3:7,9:15,37)]
  df.chars.cor <- df.chars.cor %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor <- cor(df.chars.cor, method = "pearson")
  corrplot::corrplot(df.cor,  main= "Control & Schz", type = "lower", method="number")
  
  #cont correlation
  df.chars.cor1 <- df.chars[1:32,c(3:7,9:15,37)]
  df.chars.cor1 <- df.chars.cor1 %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor1 <- cor(df.chars.cor1, method = "pearson")
  corrplot::corrplot(df.cor1, main= "Control",  type = "lower",method="number")
  
  #schz correlation
  df.chars.cor2 <- df.chars[33:54,c(3:7,9:15,37)]
  df.chars.cor2 <- df.chars.cor2 %>% mutate_all(~replace(., is.na(.), 0))
  
  df.cor2 <- cor(df.chars.cor2, method = "pearson")
  corrplot::corrplot(df.cor2, main= "Schz",  type = "lower",method="number")
  ######
  
  #node grouping
  
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
  meandf <- rbind(schz.features, cont.features)
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[27] <- "mean"
  write.csv(df.chars, "dfcharsschzcond.csv")
  
  
}

plot_as_groups <- function()

{  
  ########## plot graph according to groups
  
  
  for (i in 1:32) {
    
    
    
    if ((node.score$score[i] == 0 ) && (i != 8) && (i != 6) && (i != 22) && 
        (i != 4) && (i != 9) && (i != 24) && (i != 3) && (i != 7) &&
        (i != 18) && (i!= 2) && (i!=31))
    {
      V(network)$color[i] <- "green"
      V(network)$label.color[i] <- "white"
    }
    else if ((i == 8) || (i == 6) || (i == 22) || 
             (i == 4) || (i == 9) || (i == 24) || (i == 3) || (i == 7) ||
             (i == 18) || (i == 2) || (i == 31))
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
      V(network)$label.color[i] <- "white"
    }
    
  }
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  #V(network)$color <- "orange"
  #V(network)$label <- "" 
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  for (i in 33:54) {
    
    
    if((node.score$score[i] == 0 ) && (i != 38) && (i != 54) && (i != 48) &&
       (i != 50) && (i!= 49))
    {
      V(network)$color[i] <- "red"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
    }
    else if((i == 38) || (i == 54) || (i == 48) ||
            (i == 50) || (i == 49))
    {
      V(network)$color[i] <- "purple"
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
  
  legend('bottomleft',  c("Cont Grp#1 (score = 0)", "Cont Grp#2 (score > 0)",
                          "Schz Grp#1 (score = 0)","Schz Grp#2 (score > 0)","Singleton Group"), 
         pch=c(15,15,15,15,15), title="Groups by Score (inter/intra)",
         col=c( "red", "purple", "green", "blue", "orange"), 
         pt.bg=c("red", "purple", "green", "blue", "orange"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  #######
  
}