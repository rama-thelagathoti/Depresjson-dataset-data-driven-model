sch_analysis <- function(cor_matrix, network, exp, infl, features)
{
  
  
  exp = 3; infl = 2 ; features = "cluster"
  set.seed(1)
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  #cluster#2 = depressed
  #cluster#6 = healthy
  #cluster#8 = schz
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
  
  #for (i in 1:55)
  #{
    #print(E(tempnet)[V(tempnet)[name== paste("p",i,sep = "") ] %--% V(tempnet)])
  #}
  
  #find intra-inter cluster edges
  
  ncol = 13
  nrow = 77
  
  node.score <- data.frame(matrix(ncol = ncol, nrow = nrow))
  cols <- c("id","cluster", "deg", "intra", "inter.d","inter.s","inter.c", "score","ccoef",
            "btn", "eigen", "hubs", "auths")
  colnames(node.score) <- cols
  node.score$id <- 1:nrow
  node.score$cluster <- mcl.res$Cluster
  node.score$deg <- igraph::degree(network)
  node.score$deg [is.na(node.score$deg)] <- 0
  
  node.score[is.na(node.score)] = 0
  node.score$ccoef <- transitivity(network, type = "local")
  node.score$ccoef [is.na(node.score$ccoef)] <- 0
  
  net.matrix = get.adjacency(network, sparse=FALSE)
  # since its a 55x55 matrix , use 2 loop variable i & j
  for (i in 1:nrow) {
    
    for (j in 1:nrow) {
      # if edge present & not a loop
      if((net.matrix[i,j] == 1) && (i != j))
      {
        # same cluster then intra else inter edge
        if(node.score$cluster[i] == node.score$cluster[j]) {
          node.score$intra[i] = node.score$intra[i] + 1
        }   else if((node.score$cluster[i] == 2) && (node.score$cluster[j] == 6))   {
          #edges from depressed to control
          node.score$inter.c[i] = node.score$inter.c[i] + 1   
        } else if((node.score$cluster[i] == 2) && (node.score$cluster[j] == 8))     {
          #edges from depressed to schz
          node.score$inter.s[i] = node.score$inter.s[i] + 1   
        } else if((node.score$cluster[i] == 6) && (node.score$cluster[j] == 2))     {
          #edges from  control to depressed
          node.score$inter.d[i] = node.score$inter.d[i] + 1   
        } else if((node.score$cluster[i] == 6) && (node.score$cluster[j] == 8))     {
          #edges from control to schz
          node.score$inter.s[i] = node.score$inter.s[i] + 1   
        } else if((node.score$cluster[i] == 8) && (node.score$cluster[j] == 2))     {
          #edges from schz to depressed
          node.score$inter.d[i] = node.score$inter.d[i] + 1   
        } else if((node.score$cluster[i] == 8) && (node.score$cluster[j] == 6))     {
          #edges from schz to control
          node.score$inter.c[i] = node.score$inter.c[i] + 1   
        }
        
      }
      
    }
  }
  
  node.score$score <- (node.score$inter.d + 
                         node.score$inter.s + 
                         node.score$inter.c ) /node.score$intra
  node.score$score [is.na(node.score$score)] <- 0
  
  
  #centrality scores
  #degree, betweeness, closeness, authority_score, hub-score, eigen_Values
  #https://bookdown.org/markhoff/social_network_analysis/centrality.html#eigenvector-centrality
  btn <- round(betweenness(cor_matrix),3)
  clos <- closeness(cor_matrix)
  eigen <- round(evcent(cor_matrix),3)
  hub_score <- round(hub.score(network)$vector,3)
  auth_score <- round(authority.score(network)$vector,3)
  
  node.score$btn <- btn
  node.score$eigen <- eigen
  node.score$hubs <- hub_score
  node.score$auths <- auth_score
  
  df.chars <-  cbind(node.score, scores.merged)
  df.chars$avg <- colMeans(rbind(df.chars$madrs1, df.chars$madrs2), na.rm=TRUE)
  
  df.chars$grp <- NA
  for (i in 1:23) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- 11
    else
      df.chars$grp[i] <- 12
  }
  
  for (i in 24:55) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- 21
    else
      df.chars$grp[i] <- 22
  }
  
  for (i in 56:77) {
    
    if(df.chars$score[i] == 0)
      df.chars$grp[i] <- 31
    else
      df.chars$grp[i] <- 32
  }
  df.chars$grp <-as.factor(df.chars$grp) 
  
  
  # get mean 
  meandf <- rbind(deprs.features, cont.features,schz.features)
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[37] <- "mean"
  
  

  
  
  ########## plot graph according to groups
  
  
  for (i in 1:23) {
    
    
    
    if ((node.score$score[i] == 0 ) && (i != 2) && (i != 14) && (i != 18))
    {
      V(network)$color[i] <- "Red"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
      
    }
    else if (( i == 2) || ( i == 14) || (i == 18))
    {
      V(network)$color[i] <- "skyblue"
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
      V(network)$shape[i] <- "square"
      
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
      V(network)$shape[i] <- "circle"
    }
    else if( i == 44)
    {
      V(network)$color[i] <- "skyblue"
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
      V(network)$shape[i] <- "circle"
      
    }
  }
  
  for (i in 56:77) {
    
    
    if((node.score$score[i] == 0 ) && (i != 57))
    {
      V(network)$color[i] <- "blue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
    }
    else if( i == 57)
    {
      V(network)$color[i] <- "skyblue"
      V(network)$label.color[i] <- "white"
      V(network)$shape[i] <- "square"
      
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
    }
    else
    {
      V(network)$color[i] <- "gold"
      V(network)$label.color[i] <- "black"
      V(network)$shape[i] <- "square"
      
    }
  }
  plot(network, vertex.label.font = 2)
  #plot(network, layout_with_kk(network), vertex.label.font = 2)
  
  legend('bottomleft',  c("Depres Grp#1 (score = 0)", "Depres Grp#2 (score > 0)",
                          "Control Grp#1 (score = 0)","Control Grp#2 (score > 0)",
                          "Schz Grp#1 (score = 0)","Schz Grp#2 (score > 0)",
                          "Singleton Group"), 
         pch=c(15,15,19,19,15,15,15), title="Groups by Score (inter/intra)",
         col=c( "red", "purple", "green", "orange", "blue","gold","skyblue"), 
         pt.bg=c("red", "purple", "green", "orange", "blue","gold","skyblue"), 
         pt.cex=1.6, cex=.8, bty="n")
  
  
  #######
}

create_subgraph(network)
{
  

  subv <- c('p1','p3','p4','p5', 'p6', 'p7', 'p8', 'p9', 'p10',
            'p11','p12', 'p13','p15', 'p16', 'p17', 'p19', 'p20',
            'p21', 'p22', 'p23', 
            'p24', 'p25', 'p26', 'p27','p28','p29', 'p30',
            'p31', 'p32', 'p33', 'p34', 'p35', 'p36', 'p37','p38','p39', 'p40',
            'p41', 'p42', 'p43', 'p45', 'p46', 'p47','p48','p49', 'p50',
            'p51', 'p52', 'p53', 'p54','p55',
            'p56', 'p58', 'p59','p60', 'p61','p62','p63', 'p64','p65',
            'p66','p67','p68', 'p69','p70','p71','p72','p73','p74','p75','p76','p77'
            )
  fullg <- induced.subgraph(graph=network,vids=subv)
  plot(fullg)
  fullg.adj <- get.adjacency(fullg, sparse=FALSE)
  btn <- round(betweenness(fullg.adj),3)
  clos <- closeness(fullg.adj)
  eigen <- round(evcent(fullg.adj),3)
  hub_score <- round(hub.score(fullg)$vector,3)
  auth_score <- round(authority.score(fullg)$vector,3)
  
  fullg.df <- data.frame(subv,btn,clos,eigen,hub_score,auth_score) 
  write.csv(fullg.df,"fullgdf.csv")
  write.csv(df.chars,"dfchars.csv")
  
  
    #https://stackoverflow.com/questions/23682113/creating-subgraph-using-igraph-in-r
  
  subv <- c('p1','p3','p4','p5', 'p6', 'p7', 'p8', 'p9', 'p10',
            'p11','p12', 'p13','p15', 'p16', 'p17', 'p19', 'p20',
            'p21', 'p22', 'p23')
  
  subg.depr <- induced.subgraph(graph=network,vids=subv)
  plot(subg.depr)
  subg.depr.mat <- get.adjacency(subg.depr, sparse=FALSE)
  betweenness(subg.depr.mat)
  
  
  subv <- c('p24', 'p25', 'p26', 'p27','p28','p29', 'p30',
            'p31', 'p32', 'p33', 'p34', 'p35', 'p36', 'p37','p38','p39', 'p40',
            'p41', 'p42', 'p43', 'p45', 'p46', 'p47','p48','p49', 'p50',
            'p51', 'p52', 'p53', 'p54','p55')
  
  subg.cond <- induced.subgraph(graph=network,vids=subv)
  plot(subg.cond)
  subg.cond.mat <- get.adjacency(subg.cond, sparse=FALSE)
  betweenness(subg.cond.mat)
  

  subv <- c('p56', 'p58', 'p59','p59','p60', 'p61','p62','p63', 'p64','p65',
            'p66','p67','p68', 'p69','p70','p71','p72','p73','p74','p75','p76','p77')
  
  subg.schz <- induced.subgraph(graph=network,vids=subv)
  plot(subg.schz)
  subg.schz.mat <- get.adjacency(subg.schz, sparse=FALSE)
  betweenness(subg.schz.mat)
  
  subv <- c('p1','p3','p4','p5', 'p6', 'p7', 'p8', 'p9', 'p10',
            'p11','p12', 'p13','p15', 'p16', 'p17', 'p19', 'p20',
            'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27','p28','p29', 'p30',
            'p31', 'p32', 'p33', 'p34', 'p35', 'p36', 'p37','p38','p39', 'p40',
            'p41', 'p42', 'p43', 'p45', 'p46', 'p47','p48','p49', 'p50',
            'p51', 'p52', 'p53', 'p54','p55')
  
  d.c <- induced.subgraph(graph=network,vids=subv)
  plot(d.c)
  d.c.mat <- get.adjacency(d.c, sparse=FALSE)
  #betweenness(d.c.mat)
  
  d.c.df <- data.frame(subv,  betweenness(d.s.mat))
  
  
  subv <- c('p1','p3','p4','p5', 'p6', 'p7', 'p8', 'p9', 'p10',
            'p11','p12', 'p13','p15', 'p16', 'p17', 'p19', 'p20',
            'p21', 'p22', 'p23', 
            'p56', 'p58', 'p59','p60', 'p61','p62','p63', 'p64','p65',
            'p66','p67','p68', 'p69','p70','p71','p72','p73','p74','p75','p76','p77')
  
  d.s <- induced.subgraph(graph=network,vids=subv)
  plot(d.s)
  d.s.mat <- get.adjacency(d.s, sparse=FALSE)
  betweenness(d.s.mat)
  
  df <- data.frame(subv,  betweenness(d.s.mat))
  
  subv <- c('p24', 'p25', 'p26', 'p27','p28','p29', 'p30',
            'p31', 'p32', 'p33', 'p34', 'p35', 'p36', 'p37','p38','p39', 'p40',
            'p41', 'p42', 'p43', 'p45', 'p46', 'p47','p48','p49', 'p50',
            'p51', 'p52', 'p53', 'p54','p55',
            'p56', 'p58', 'p59','p59','p60', 'p61','p62','p63', 'p64','p65',
            'p66','p67','p68', 'p69','p70','p71','p72','p73','p74','p75','p76','p77')
  
  c.s <- induced.subgraph(graph=network,vids=subv)
  plot(c.s)
  c.s.mat <- get.adjacency(c.s, sparse=FALSE)
  betweenness(c.s.mat)
}