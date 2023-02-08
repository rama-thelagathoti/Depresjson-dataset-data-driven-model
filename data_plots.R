#############################################################################
# File name: data_plots.R
# Author: Rama Thelagathoti
# Description: plot utility functions
############################################################################

library(dplyr)
library(ggraph)
library(graphlayouts)
library(ggcorrplot)
library(networkD3)

# number of Days observed for each patient
plot(1:77, scores.merged$days, pch = 16, col = "blue", cex = 1.2 ,type = "o",
     main = "No of days of motor data available ", xlab = "Subject id", ylab = "number of days")
axis(1, 1:77)
axis(2, 1:20)
abline(h=1:20, v=1:77, col="gray", lty=3)

#Gender percentage
male <- scores.merged %>% 
  group_by(gender) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(percentage = prop.table(Count)*100)
p <- ggplot(male, aes(reorder(gender, -percentage), percentage), fill = male)+
  geom_col(fill = c("orange", "red"))+
  geom_text(aes(label = sprintf("%.2f%%", percentage)))+
  xlab("Gender") + 
  ylab("Percent")+
  ggtitle("Gender Percentage")
show(p)


age <- scores.merged %>% 
  group_by(age) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(percentage = prop.table(Count)*100)
p <- ggplot(age, aes(reorder(age, -percentage), percentage), fill = age)+
  geom_col(fill = "orange")+
  geom_text(aes(label = sprintf("%.2f%%", percentage)))+
  xlab("age range") + 
  ylab("Percent")+
  ggtitle("Age range Percentage")
show(p)

# total patients
tp <-  scores.merged %>% 
  group_by(type) %>% 
  dplyr::summarise(Count = n()) 
p <- ggplot(tp, aes(reorder(type, -Count), Count), fill = tp)+
  geom_col(fill = c("green", "red","orange"))+
  geom_text(aes(label = sprintf("%d", Count)))+
  xlab("Subject type") + 
  ylab("total")+
  ggtitle("Total subjects category")
show(p)


############################################################################
# Function: plot_scores
# Description: plot exploratory data of scores dataset
#############################################################################
plot_scores <- function(scores)
{
  p <- ggplot(scores, aes(x= number, y = days, fill = "red")) +
    geom_bar(position="dodge", stat="identity", fill = "#FF6666")
  show(p)
  
  p <- ggplot(scores, aes(x= gender, y = gender), fill = number) +
    geom_bar(position="dodge", stat="identity",  fill = "#FF6666") +
    ggtitle("Gender") +
    xlab("Gender type") 
  show(p)
  
  male <- scores %>% 
    group_by(gender) %>% 
    dplyr::summarise(Count = n()) %>% 
    mutate(percentage = prop.table(Count)*100)
  p <- ggplot(male, aes(reorder(gender, -percentage), percentage), fill = male)+
    geom_col(fill = c("green", "red"))+
    geom_text(aes(label = sprintf("%.2f%%", percentage)))+
    xlab("Gender") + 
    ylab("Percent")+
    ggtitle("Gender Percentage")
  show(p)
  
  afftype <- scores %>% 
    group_by(afftype) %>% 
    dplyr::summarise(Count = n()) %>% 
    mutate(percentage = prop.table(Count)*100)
  p <- ggplot(afftype, aes(reorder(afftype, -percentage), percentage), fill = afftype)+
    geom_col(fill = "green")+
    geom_text(aes(label = sprintf("%.2f%%", percentage)))+
    xlab("afftype") + 
    ylab("Percent")+
    ggtitle("afftype Percentage")
  show(p)
  
  melanch <- scores %>% 
    group_by(melanch) %>% 
    dplyr::summarise(Count = n()) %>% 
    mutate(percentage = prop.table(Count)*100)
  p <- ggplot(melanch, aes(reorder(melanch, -percentage), percentage), fill = melanch)+
    geom_col(fill = "green")+
    geom_text(aes(label = sprintf("%.2f%%", percentage)))+
    xlab("melanch") + 
    ylab("Percent")+
    ggtitle("melanch Percentage")
  show(p)
  
  age <- scores %>% 
    group_by(age) %>% 
    dplyr::summarise(Count = n()) %>% 
    mutate(percentage = prop.table(Count)*100)
  p <- ggplot(age, aes(reorder(age, -percentage), percentage), fill = age)+
    geom_col(fill = "green")+
    geom_text(aes(label = sprintf("%.2f%%", percentage)))+
    xlab("age range") + 
    ylab("Percent")+
    ggtitle("Age range Percentage")
  show(p)
  
  inpatient <- scores %>% 
    group_by(inpatient) %>% 
    dplyr::summarise(Count = n()) %>% 
    mutate(percentage = prop.table(Count)*100)
  ggplot(inpatient, aes(reorder(inpatient, -percentage), percentage), fill = inpatient)+
    geom_col(fill = "green")+
    geom_text(aes(label = sprintf("%.2f%%", percentage)))+
    xlab("Patient type") + 
    ylab("Percent")+
    ggtitle("Patient Percentage")

  #plot madrs before and after assessment
  deprs_scores <- scores[1:23, ]
  pal <- colorRampPalette(colors = c("lightblue", "blue"))(3)
  deprs_data <- t(data.frame(deprs_scores$madrs1,deprs_scores$madrs2))
  barplot(deprs_data, 
          legend = c("madrs1", "madrs2"), 
          names.arg=deprs_scores$number, 
          beside=TRUE,
          col=pal,
          xlab="number", 
          ylab="madrs score", 
          main="madrs scores comparision", 
          ylim=c(0,40))
}

############################################################################
# Function: plot_days
# Description: visualize number of days and patients category
#############################################################################

plot_days <- function(scores)
{
  # total patients
  tp <-  scores %>% 
    group_by(type) %>% 
    dplyr::summarise(Count = n()) 
  p <- ggplot(tp, aes(reorder(type, -Count), Count), fill = tp)+
    geom_col(fill = c("green", "red"))+
    geom_text(aes(label = sprintf("%d", Count)))+
    xlab("Patient type") + 
    ylab("total")+
    ggtitle("Total subjects category")
  show(p)
  # number of Days observed for each patient
  plot(1:55, scores$days, pch = 16, col = "red", cex = 1.2 ,type = "o",
       main = "Days observed for each patient", xlab = "patient id", ylab = "number of days")
  axis(1, 1:55)
  axis(2, 1:20)
  abline(h=1:20, v=1:55, col="gray", lty=3)
  
}


############################################################################
# Function: plot_group_activity
# Description: visualize mean activity between condition and control groups
#############################################################################
plot_group_activity <- function(final_df)
{
  
  final_df <- df.features
  df_day_mean <- final_df
  df_day_mean$day <- as.numeric(df_day_mean$day)
  df_day_mean$id <- as.numeric(df_day_mean$id)
  df_day_mean$day_mean <-  apply(df_day_mean[ , c(4:27)], 1, mean)
  df_day_mean$day_mean <- round(df_day_mean$day_mean)
  str(df_day_mean)
  colSums(is.na(df_day_mean))
  c1 <- subset(df_day_mean, df_day_mean$id == 1 )
  c1$day_mean <- apply(c1[ , c(4:27)], 1, mean)
  c1$day_mean <- round(c1$day_mean)
  
  c2 <- subset(df_day_mean, df_day_mean$id == 2 )
  c2$day_mean <- apply(c2[ , c(4:27)], 1, mean)
  c2$day_mean <- round(c2$day_mean)
  
  c24 <- subset(df_day_mean, df_day_mean$id == 24 )
  c24$day_mean <- apply(c24[ , c(4:27)], 1, mean)
  c24$day_mean <- round(c24$day_mean)
  
  c25 <- subset(df_day_mean, df_day_mean$id == 25 )
  c25$day_mean <- apply(c25[ , c(4:27)], 1, mean)
  c25$day_mean <- round(c25$day_mean)
  
  x1 <- c1$day
  y1 <- c1$day_mean
  
  x2 <- c2$day
  y2 <- c2$day_mean
  
  x3 <- c24$day
  y3 <- c24$day_mean
  
  x4 <- c25$day
  y4 <- c25$day_mean
  
  min_valuex <- min (x1,x2, x3, x4)
  max_valuex <- max (x1,x2, x3, x4)
  min_valuey <- min(y1,y2, y3, y4)
  max_valuey <- max(y1,y2, y3, y4)
  matplot(x1 ,y1 , xlim = c(min_valuex, max_valuex), ylim = c(min_valuey, max_valuey), type = "l", col = "red" , main = "activity among the patients", xlab = "day & date ", ylab = "activity")
  matplot(x2 ,y2 ,  type = "l", col = "green", add = TRUE)
  matplot(x3 ,y3 ,  type = "l", col = "blue", add = TRUE)
  matplot(x4 ,y4 ,  type = "l", col = "black", add = TRUE)
  
  legend("topright",legend=c("c1","c2","c24", "c25"),col = c("red", "green", "blue", "black"), lty = 1, cex = 0.6)
  
  
  # condition vs control group plots
  
  #
  d <- df_day_mean %>%
    group_by(id,day) %>%
    summarise(id = id, day = day, day_mean = day_mean)
  
  
  y <- c(0,max(d$day_mean))
  y
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
  
  for (i in 24:55) {
    
    x <- subset(d$day, d$id == i )
    y <- subset(d$day_mean, d$id == i )
    lines(x, y, type = "l", col = "blue" )  
    
  }
  
  legend("topright",legend=c("condition", "control"),col = c("red", "blue"), lty = 1, cex = 0.6)

  # plot isolated node vs connected node
  
  x <- subset(d$day, d$id == 1 )
  y <- subset(d$day_mean, d$id == 1 )
  plot(x, y, type = "l", col = "red", xlim = c(1,max(d$day)), ylim = c(0,max(d$day_mean)),
       main = "condition vs control mean activity",
       xlab = "days", ylab = "mean activity")  

  for (i in 1:23) {
    
    x <- subset(d$day, d$id == i )
    y <- subset(d$day_mean, d$id == i )
    lines(x, y, type = "l", col = "red" )  
    
  }
  
  # hourly - 2, 14, 18
  x <- subset(d$day, d$id == 2 )
  y <- subset(d$day_mean, d$id == 2 )
  lines(x, y, type = "l",lw = 2,  col = "blue" )  

  x <- subset(d$day, d$id == 14 )
  y <- subset(d$day_mean, d$id == 14 )
  lines(x, y, type = "l",lw = 2, col = "blue" )   

  x <- subset(d$day, d$id == 18 )
  y <- subset(d$day_mean, d$id == 18 )
  lines(x, y, type = "l", lw = 2,col = "blue" )   
  
  x <- subset(d$day, d$id == 20 )
  y <- subset(d$day_mean, d$id == 20 )
  lines(x, y, type = "l", lw = 2,col = "green" )   
  
  x <- subset(d$day, d$id == 21 )
  y <- subset(d$day_mean, d$id == 21 )
  lines(x, y, type = "l", lw = 2,col = "black" )   
  
  
   
  # day-wise - 14 15 25 32 
  
  
  #total mean of each subject


  tot.mean <- aggregate(dd$day_mean,list(dd$id), FUN=mean)
  
}

##################################################################
# Function : plot_corr_graph
# Description: visualize correlation network
##################################################################

plot_corr_graph <- function(network, experiment)
{
  colrs <- c("gray50", "tomato", "gold")
  
  V(network)$color <- colrs[V(network)$media.type]
  V(network)$size <- V(network)$audience.size*0.7
  V(network)$label.color <- "black"
  V(network)$label <- rownames
  
  E(network)$width <- E(network)$weight/6
  E(network)$arrow.size <- .2
  E(network)$edge.color <- "gray50"
  E(network)$width <- 1+E(network)$weight/12
  
  E(network)
  str(V(network))
  V(network)$name
  V(network)$group <- c(rep(1, times= 23), rep(2, times= 32))
  V(network)$group
  
  if(experiment == 1)
    title = "Exp 1: using hourly mean"
  else if (experiment == 2)
    title = "Exp 2: using all features (hourly mean, overall mean, median & SD)"
  else if (experiment == 3)
    title = "Exp 3: using overall mean, median & SD"
  else if (experiment == 4)
    title = "Exp 4: using hourly mean and hourly sd"
  else if (experiment == 5)
    title = "Exp 5: normalization together & using all features"
  else if (experiment == 6)
    title = " features: day wise mean"
  
  ### use different layouts to show graph network
  
  plot(network, 
       vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32)),
       layout = layout_nicely,
       main=paste("Default layout",sep="" ))
       #rescale = FALSE, ylim=c(1,5),xlim=c(0,1), asp = 0)
  plot( network, layout = layout.reingold.tilford,
        edge.width = 1,
        edge.arrow.width = 0.3,
        vertex.size = 5,
        edge.arrow.size = 0.5,
        vertex.size2 = 3,
        vertex.label.cex = 1,
        asp = 0.35,
        margin = -0.1,
        vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32)),
        main=paste("reingold.tilford layout"))
  
  plot(network, vertex.size=8, 
       vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32)), 
       #layout=igraph::layout.fruchterman.reingold(network, niter=10000),
       main=paste(" with reduced vertex size"))
  
  legend(x=-1, y=-1, c("control", "condition"), pch=21, title=title,
         col=c( "pink", "skyblue"), pt.bg=c( "pink", "skyblue"), 
         pt.cex=2, cex=.8, bty="n", ncol=1)
  
  
 if (0)
   {
   plot(network,
       layout = layout_with_fr,
       vertex.label=toupper(1:16),
       vertex.size = 20,
       vertex.shape = "square",
       vertex.color="white",
       vertex.frame.color= "black",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       vertex.label.cex=1,
       edge.width=2,
       edge.color="black")
 }
  #add_layout_, component_wise, layout_as_bipartite, layout_as_star, layout_as_tree, 
  #layout_in_circle, layout_nicely, layout_on_grid, layout_on_sphere, layout_randomly, 
  #layout_with_dh, layout_with_fr, layout_with_gem, layout_with_graphopt, 
  #layout_with_kk, layout_with_lgl, layout_with_mds, layout_with_sugiyama, 
  #merge_coords, norm_coords, normalize

  
  }

##################################################################
# Function : plot_visnetwork
# Description: visualize correlation network using visnetwork package
##################################################################
plot_visnetwork <- function(network)
{
  dummy <- toVisNetworkData(network)
  my.edges <- dummy$edges
  my.nodes <- dummy$nodes
  my.nodes$groups <- c(rep(1, times= 23), rep(2, times= 32))
  my.nodes$color.background <- c("pink", "skyblue")[my.nodes$groups]
  my.nodes$color.border <- c("black", "black")[my.nodes$groups]
  #my.nodes$size = 60
  my.nodes <- my.nodes %>% mutate(font.size = 30) 
  my.edges$color <- "black"
  my.nodes$shape <- 'circle'
  my.nodes$font.size <- 60
  
  visNetwork(my.nodes, my.edges, 
             main = list(text = "Using day-wise mean & SD",
                         style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;")) %>%
    #visIgraphLayout(type = "full")
    visIgraphLayout(layout = "layout_with_mds") %>%
    visLegend(addNodes = list(
      list(label = "Cond", shape = "circle", color = "pink", size = 25),
      list(label = "Cont", shape = "circle", color = "skyblue", size = 25)), 
      useGroups = FALSE,
      width = 0.1, position = "right", main = "Legend") 
}

##################################################################
# Function : plot_corr_graph_with_schz
# Description: visualize correlation network
##################################################################

plot_corr_graph_with_schz <- function(network, experiment)
{
  colrs <- c("gray50", "tomato", "gold")
  
  V(network)$color <- colrs[V(network)$media.type]
  V(network)$size <- V(network)$audience.size*0.7
  V(network)$label.color <- "black"
  V(network)$label <- rownames.77
  
  E(network)$width <- E(network)$weight/6
  E(network)$arrow.size <- .2
  E(network)$edge.color <- "gray50"
  E(network)$width <- 1+E(network)$weight/12
  
  E(network)
  str(V(network))
  V(network)$name
  V(network)$group <- c(rep(1, times= 23), rep(2, times= 32), rep(3, times= 22))
  V(network)$group
  
  if(experiment == 1)
    title = "Exp 1: using hourly mean"
  else if (experiment == 2)
    title = "Exp 2: using all features (hourly mean, overall mean, median & SD)"
  else if (experiment == 3)
    title = "Exp 3: using overall mean, median & SD"
  else if (experiment == 4)
    title = "Exp 4: using hourly mean and hourly sd"
  else if (experiment == 5)
    title = "Exp 5: normalization together & using all features"
  else if (experiment == 6)
    title = " features: day wise mean"
  
  ### use different layouts to show graph network
  
  plot(network, 
       vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32), rep("lightgreen", times= 22)),
       layout = layout_nicely)
  #rescale = FALSE, ylim=c(1,5),xlim=c(0,1), asp = 0)
  plot( network, layout = layout.reingold.tilford,
        edge.width = 1,
        edge.arrow.width = 0.3,
        vertex.size = 5,
        edge.arrow.size = 0.5,
        vertex.size2 = 3,
        vertex.label.cex = 1,
        asp = 0.35,
        margin = -0.1,
        vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32), rep("lightgreen", times= 22)))
  legend(x=-1, y=-1, c("Healthy", "Depressed","schizophrenia"), pch=21, title=title,
         col=c( "pink", "skyblue", "lightgreen"), pt.bg=c( "pink", "skyblue", "lightgreen"), 
         pt.cex=2, cex=.8, bty="n", ncol=1)
  
  plot(network,
       layout = layout_nicely,
       vertex.label=toupper(1:16),
       vertex.size = 20,
       vertex.shape = "square",
       vertex.color="white",
       vertex.frame.color= "black",
       vertex.label.color = "black",
       vertex.label.family = "sans",
       vertex.label.cex=1,
       edge.width=2,
       edge.color="black",
  )
  #add_layout_, component_wise, layout_as_bipartite, layout_as_star, layout_as_tree, 
  #layout_in_circle, layout_nicely, layout_on_grid, layout_on_sphere, layout_randomly, 
  #layout_with_dh, layout_with_fr, layout_with_gem, layout_with_graphopt, 
  #layout_with_kk, layout_with_lgl, layout_with_mds, layout_with_sugiyama, 
  #merge_coords, norm_coords, normalize
  
  
}

##################################################################
# Function : plot_visnetwork_with_schz
# Description: visualize correlation network using visnetwork package
##################################################################
plot_visnetwork_with_schz <- function(network)
{
  dummy <- toVisNetworkData(network)
  my.edges <- dummy$edges
  my.nodes <- dummy$nodes
  my.nodes$groups <- c(rep(1, times= 23), rep(2, times= 32), rep(3, times= 22))
  my.nodes$color.background <- c("pink", "skyblue", "lightgreen")[my.nodes$groups]
  my.nodes$color.border <- c("black", "black","black")[my.nodes$groups]
  #my.nodes$size = 60
  my.nodes <- my.nodes %>% mutate(font.size = 30) 
  my.edges$color <- "black"
  my.nodes$shape <- 'circle'
  my.nodes$font.size <- 60
  
  visNetwork(my.nodes, my.edges, 
             main = list(text = "Using day-wise mean & SD",
                         style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;")) %>%
    #visIgraphLayout(type = "full")
    visIgraphLayout(layout = "layout_with_mds") %>%
    visLegend(addNodes = list(
      list(label = "Cond", shape = "circle", color = "pink", size = 25),
      list(label = "Cont", shape = "circle", color = "skyblue", size = 25)), 
      useGroups = FALSE,
      width = 0.1, position = "right", main = "Legend") 
}

##################################################################
# Function : cluster_MST-kNN
# Description: apply MST-kNN graph clustering algorithm 
##################################################################

cluster_MST_kNN <- function(cor.matrix)
{

  #knn graph clustering
  results <- mst.knn(cor.matrix)
    
  igraph::V(results$network)$label.cex <- seq(0.6,0.6,length.out=2)
  
  
  plot(results$network, vertex.size=8, 
       vertex.color=c(rep("skyblue", times= 23), rep("pink", times= 32)), 
       layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
       main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))

  #plot(results$network, vertex.size=8, 
   #    vertex.color=igraph::clusters(results$network)$membership, 
    #   layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
     #s  main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))
  
}


