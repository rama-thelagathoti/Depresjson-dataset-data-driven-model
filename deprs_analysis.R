#############################################################################
# File name: 
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
Model_depre_control_hourly <- function(deprs.features, cont.features)
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

node_analysis()
{
  
exp = 2; infl = 2 ; 
mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
               allow1 = TRUE,max.iter = 100, ESM = FALSE)
print(mcl.res)

#find degree of each node and stote it in a vector
deg <- igraph::degree(network, mode="all")
t <- as.data.frame(deg)
degree <- t$deg


#find intra-inter cluster edges

nrow = 55
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
#node.score$coef[1:23] <- round(transitivity(network, vids = controls, type = "local"), 3)
#node.score$coef[24:55] <- round(transitivity(network, vids = schzs, type = "local"), 3)
#node.score$ccoef [is.na(node.score$ccoef)] <- 0

citmat <- cocitation(network)
#intra
citmat.depr <- citmat[1:23,1:23]
citmat.depr.sums <- rowSums(citmat.depr)
citmat.depr.sums <- as.data.frame(citmat.depr.sums)
citmat.cont <- citmat[24:55,24:55]
citmat.cont.sums <- rowSums(citmat.cont)
citmat.cont.sums <- as.data.frame(citmat.cont.sums)
node.score$cointra <- append((citmat.cont.sums$citmat.cont.sums),(citmat.depr.sums$citmat.depr.sums))
#inter
citmat.depr <- citmat[1:23,24:55]
citmat.depr.sums <- rowSums(citmat.depr)
citmat.depr.sums <- as.data.frame(citmat.depr.sums)
citmat.cont <- citmat[24:55,1:23]
citmat.cont.sums <- rowSums(citmat.cont)
citmat.cont.sums <- as.data.frame(citmat.cont.sums)
node.score$cointer <- append((citmat.cont.sums$citmat.cont.sums),(citmat.depr.sums$citmat.depr.sums))
node.score$coscr <- (node.score$cointer/node.score$cointra)

df.chars <-  cbind(node.score, scores.merged[1:55,])

# get mean 
meandf <- rbind(deprs.features, cont.features)
meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
df.chars <- cbind(df.chars,meandf$mean)
colnames(df.chars)[37] <- "mean"

#write.csv(df.chars, "dfchars_cont_schz_6_1.csv")

#all correlation
df.chars.cor <- df.chars[,c(3:7,9:15,37)]
df.chars.cor <- df.chars.cor %>% mutate_all(~replace(., is.na(.), 0))

df.cor <- cor(df.chars.cor, method = "pearson")
corrplot::corrplot(df.cor,  main= "Control & Schz",  type = "lower",method="number")

#depr correlation
df.chars.cor1 <- df.chars[1:23,c(3:7,9:15,37)]
df.chars.cor1 <- df.chars.cor1 %>% mutate_all(~replace(., is.na(.), 0))

df.cor1 <- cor(df.chars.cor1, method = "pearson")
corrplot::corrplot(df.cor1, main= "Control", type = "lower", method="number")

#control correlation
df.chars.cor2 <- df.chars[24:55,c(3:7,9:15,37)]
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


depr_analysis <- function(deprs.features, cont.features)
{
  
  exp = 2; infl = 2 ; features = "cluster"
  
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
                 allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)
  
  deg <- igraph::degree(network, mode="all")

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
  
  
  node.score <- data.frame(matrix(ncol = 11, nrow = 55))
  cols <- c("id","cluster", "deg", "intra", "inter", "score",
            "btn", "eigen", "hubs", "auths","ccoef")
  colnames(node.score) <- cols
  node.score$id <- 1:55
  node.score$cluster <- mcl.res$Cluster
  node.score$deg <- igraph::degree(network)
  node.score$deg [is.na(node.score$deg)] <- 0
  
  btn <- round(betweenness(cor_matrix),3)
  clos <- closeness(cor_matrix)
  eigen <- round(evcent(cor_matrix),3)
  hub_score <- round(hub.score(network)$vector,3)
  auth_score <- round(authority.score(network)$vector,3)
  
  node.score$btn <- btn
  node.score$eigen <- eigen
  node.score$hubs <- hub_score
  node.score$auths <- auth_score
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
  

  # get mean 
  meandf <- rbind(deprs.features, cont.features)
  meandf <- aggregate(meandf[ ,c(1,52)], list(meandf$id), mean)
  df.chars <- cbind(df.chars,meandf$mean)
  colnames(df.chars)[27] <- "mean"
  write.csv(df.chars, "dfchars_Deprs.csv")
  
  
  subv <- c('p1','p3','p4','p5', 'p6', 'p7', 'p8', 'p9', 'p10',
            'p11','p12', 'p13','p15', 'p16', 'p17', 'p19', 'p20',
            'p21', 'p22', 'p23', 
            'p24', 'p25', 'p26', 'p27','p28','p29', 'p30',
            'p31', 'p32', 'p33', 'p34', 'p35', 'p36', 'p37','p38','p39', 'p40',
            'p41', 'p42', 'p43', 'p45', 'p46', 'p47','p48','p49', 'p50',
            'p51', 'p52', 'p53', 'p54','p55')
  deprg <- induced.subgraph(graph=network,vids=subv)
  plot(deprg)
  deprg.adj <- get.adjacency(deprg, sparse=FALSE)
  btn <- round(betweenness(deprg.adj),3)
  clos <- closeness(deprg.adj)
  eigen <- round(evcent(deprg.adj),3)
  hub_score <- round(hub.score(deprg)$vector,3)
  auth_score <- round(authority.score(deprg)$vector,3)
  
  deprg.df <- data.frame(subv,btn,clos,eigen,hub_score,auth_score) 
  write.csv(deprg.df,"deprg.csv")
  
}