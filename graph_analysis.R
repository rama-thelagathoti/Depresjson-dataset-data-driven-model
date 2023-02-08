#############################################################################
# File name: graph_analysis.R
# Author: Rama Thelagathoti
# Description: clustering and analysis
############################################################################

############################################################################
# Function: mcl_clst_analysis
# Description: 
#############################################################################
mcl_clst_analysis <- function(mcl.clst, df.scores)
{
   #https://rkabacoff.github.io/datavis/Bivariate.html
   mcl.clst <- mcl.res
  
  df.clts <- cbind(df.scores, mcl.clst$Cluster)
  colnames(df.clts)[15] <- "cluster"
  df.clts$id <- 1:55
  df.clts$cluster <- as.factor(df.clts$cluster)
  df.clts$days <- as.factor(df.clts$days)
  

  df.clts %>% 
    group_by(cluster) %>%
    summarise(number, cluster)
    
  p <- ggplot(df.clts, aes(x = cluster, y = id, fill = cluster)) +
    geom_point(shape=4, size = 3) +
    scale_y_continuous("id",breaks=seq(0, 55, 1)) 
    
  show(p)
  
  #p <- ggplot(df.clts, aes(cluster, ..count..)) + geom_bar(aes(fill = age), stat='count', position = "stack") 
  #show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=age)) + geom_bar(aes(fill = age)) +
    geom_text(aes(label=..count..), stat='count', position="stack") +
    scale_fill_manual(values = c("darkred", "steelblue", 
                                 "darkgreen", "gold",
                                 "brown", "purple", 
                                 "grey", "khaki4",
                                 "green", "black"))
  show(p)
  
  

  p <- ggplot(df.clts, aes(cluster, fill=madrs1)) + geom_bar(aes(fill = madrs1)) +
    geom_text(aes(label=..count..), stat='count', position="stack") +
    scale_fill_brewer(palette = "Dark2") 
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=type)) + geom_bar(aes(fill = type)) +
    geom_text(aes(label=type), stat='count', position="stack", size = 5) +
    ggtitle("Community Analysis: condition vs control distribution") +
    xlab("community id") + ylab("number of control and condition persons") 
  p + theme(axis.title = element_text(size = 50))
  p + theme(axis.text = element_text(size = 55)) 
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=age)) + geom_bar(aes(fill = age)) +
    geom_text(aes(label = paste(format(age ), "=")), stat='count', position="stack",,vjust=1.5, size = 6)+
    geom_text(aes(label=..count..), stat='count', position="stack",  size = 6, hjust = -4.5,vjust=1.5)+ 
    ggtitle("Community Analysis: Age distribution") +
    xlab("community id") + ylab("number of persons with age group ") +
    scale_color_brewer(palette="Set2")
    #p + theme(text = element_text(size = 6))
    #p + theme(axis.title.x = element_text(size = 10))
    #p + theme(axis.text = element_text(size = 6))        
    #p + scale_color_brewer(palette="Set1")

  show(p)
  
      
  
  ggplot(age.dist, 
         aes(x = cluster, 
             fill = age)) + 
    geom_bar(position = "stack") +
    geom_text(aes(label = lbl), size = 3, stat='count',
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") 
    
  
  
  p <- ggplot(df.clts, aes(cluster, fill=gender)) + geom_bar(aes(fill = gender)) +
    geom_text(aes(label=after_stat(..count..)), stat='count', position="stack") +
    ggtitle("Community Analysis: Gender distribution") +
    xlab("community id") + ylab("number of persons") 
  p + theme(axis.title = element_text(size = 20))
  p + theme(axis.text = element_text(size = 10))  
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=days)) + geom_bar(aes(fill = days)) +
    geom_text(aes(label=days), stat='count', position="stack") +
    ggtitle("Community Analysis: days of assessment distribution") +
    xlab("community id") + ylab("number of days monitored") 
  p + theme(axis.title = element_text(size = 20))
  p + theme(axis.text = element_text(size = 10)) 
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=afftype)) + geom_bar(aes(fill = afftype)) +
    geom_text(aes(label=afftype), stat='count', position="stack") +
    ggtitle("Community Analysis: affinity type distribution") +
    xlab("community id") + ylab("number of persons with affinity") 
  p + theme(axis.title = element_text(size = 20))
  p + theme(axis.text = element_text(size = 10)) 
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=melanch)) + geom_bar(aes(fill = melanch)) +
    geom_text(aes(label=melanch), stat='count', position="stack")
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=inpatient)) + geom_bar(aes(fill = inpatient)) +
    geom_text(aes(label=inpatient), stat='count', position="stack")
  show(p)

  p <- ggplot(df.clts, aes(cluster, fill=madrs1)) + geom_bar(aes(fill = madrs1)) +
    geom_text(aes(label=days), stat='count', position="stack") +
    ggtitle("Community Analysis: days of assessment distribution") +
    xlab("community id") + ylab("number of days monitored") 
  p + theme(axis.title = element_text(size = 20))
  p + theme(axis.text = element_text(size = 10)) 
  show(p)
  
  
  ################# New
  
  age.dist <- df.clts %>%
    dplyr::group_by(cluster, age) %>%
    dplyr::summarize(n = n()) %>% 
    dplyr::mutate(pct = n/sum(n),
                  lbl = scales::percent(pct))
  
  p <- ggplot(age.dist, aes( x = cluster, fill=age)) +
    geom_bar(position = "stack") +
    geom_text(aes(label = lbl),stat='count', 
              size = 5, position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    labs(y = "Percent",fill = "Age", x = "Community id",
         title = "Community Analysis: Age distribution") +
    theme(axis.text.y=element_blank()) +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position = "none")
  show(p)
  
  p <- ggplot(df.clts, aes( x = cluster, fill=age)) +
    geom_bar(position = "stack") +
    geom_text(aes(label = lbl),stat='count', 
              size = 5, position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    labs(y = "Percent",fill = "Age", x = "Community id",
         title = "Community Analysis: Age distribution") +
    theme(axis.text.y=element_blank()) +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position = "none")
  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=age)) + geom_bar(aes(fill = age)) +
    geom_text(aes(label = paste(format(age ), "=")), stat='count', position="stack",,vjust=1, size = 5)+
    geom_text(aes(label=..count..), stat='count', position="stack",  size = 5, hjust = -4.5,vjust=1)+ 
    scale_fill_brewer(palette = "Set3") +
    labs(y = "Age",fill = "Age", x = "Community id",
         title = "Community Analysis: Age distribution") +
    theme(axis.text.y=element_blank()) +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position =   c(0.1, 0.8))   +
    theme(legend.text=element_text(size=15)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #+
    #theme(legend.position = "right", legend.background = element_rect(fill="gray95"))
    
  
  show(p)
  
  

  
  p <- ggplot(df.clts, aes(cluster, fill=type)) + geom_bar(aes(fill = type)) +
    geom_text(aes(label=type), stat='count', position="stack", size = 5) +
    ggtitle("Community Analysis: condition vs control distribution") +
    xlab("community id") + ylab("group") +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position =   c(0.1, 0.8))   +
    theme(legend.text=element_text(size=15)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    #theme(legend.position = "right", legend.background = element_rect(fill="gray95"))
  
  
  show(p)
  
  
  p <- ggplot(df.clts, aes(cluster, fill=gender)) + geom_bar(aes(fill = gender)) +
    #geom_text(aes(label=after_stat(..count..)), stat='count', position="stack") +
    geom_text(aes(label = paste(format(gender), "=")), stat='count', position="stack",,vjust=1.5, size = 4)+
    geom_text(aes(label=..count..), stat='count', position="stack",  size = 4, hjust = -3.5,vjust=1.5)+ 
    ggtitle("Community Analysis: Gender distribution") +
    xlab("community id") + ylab("gender") +
  theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position =   c(0.1, 0.8))   +
    theme(legend.text=element_text(size=15)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  show(p)
  
  p <- ggplot(df.clts, aes(cluster, fill=days)) + geom_bar(aes(fill = days)) +
    #geom_text(aes(label=days), stat='count', position="stack", size = 5) +
    ggtitle("Community Analysis: days of assessment distribution") +
    xlab("community id") + ylab("number of days monitored") +
  theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))  +
    theme(plot.title = element_text(size =15)) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set3") +
    theme(legend.position =   c(0.1, 0.8))   +
    theme(legend.text=element_text(size=15)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
    show(p)
  
    #levels(df.clts$afftype) <- c("bipolar", "bipolar", "condition", "unipolar")
    
    p <- ggplot(df.clts, aes(cluster, fill=afftype)) + geom_bar(aes(fill = afftype)) +
      geom_text(aes(label=afftype), stat='count', position="stack", vjust=1) +
      ggtitle("Community Analysis: affinity type distribution") +
      xlab("community id") + ylab("affinity type") +
    theme(axis.title = element_text(size = 15)) +
      theme(axis.text = element_text(size = 15))  +
      theme(plot.title = element_text(size =15)) +
      theme(legend.position =   c(0.1, 0.8))   +
      theme(legend.text=element_text(size=15)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
      show(p)
      
      
      ####### plot edges
      n1 <- 0
      l1 <- c()
      ld1 <- c()
      n2 <- 0
      l2 <- c()
      ld2 <- c()
      n3 <- 0
      l3 <- c()
      ld3 <- c()
      n4 <- 0
      l4 <- c()
      ld4 <- c()
      n5 <- 0
      l5 <- c()
      ld5 <- c()
      n6 <- 0
      l6 <- c()
      ld6 <- c()
      
      cnt <- df.clts %>%
        dplyr::group_by(cluster) %>%
        dplyr::summarise(n = n()) %>%
        mutate(freq = n)
      
      for (i in 1:55) {

        if(df.clts$cluster[i] == 1)
        {
          n1 <- n1 + length(neighbors(network, i))
          l1 <- c(l1,length(neighbors(network, i)))
          ld1 <- c(ld1, gsub(" ", "", paste("p",i)))
        }
        if(df.clts$cluster[i] == 2)
        {
          n2 <- n2 + length(neighbors(network, i))
          l2 <- c(l2,length(neighbors(network, i)))
          ld2 <- c(ld2,  gsub(" ", "", paste("p",i)))
          
        }
        if(df.clts$cluster[i] == 3)
        {
          n3 <- n3 + length(neighbors(network, i))
          l3 <- c(l3,length(neighbors(network, i)))
          ld3 <- c(ld3,  gsub(" ", "", paste("p",i)))
          
        }
        if(df.clts$cluster[i] == 4)
        {
          n4 <- n4 + length(neighbors(network, i))
          l4 <- c(l4,length(neighbors(network, i)))
          ld4 <- c(ld4,  gsub(" ", "", paste("p",i)))
          
        }
        if(df.clts$cluster[i] == 5)
        {
          n5 <- n5 + length(neighbors(network, i))
          l5 <- c(l5,length(neighbors(network, i)))
          ld5 <- c(ld5,  gsub(" ", "", paste("p",i)))
          
        }
        if(df.clts$cluster[i] == 6)
        {
          n6 <- n6 + length(neighbors(network, i))
          l6 <- c(l6,length(neighbors(network, i)))
          ld6 <- c(ld6,  gsub(" ", "", paste("p",i)))
        }
        
      }
      
      df.edges <- do.call(rbind, Map(data.frame, A=ld6, B=l6))
      
        ggplot(df.edges, aes(x=A, y=B)) +
        geom_bar(stat="identity", fill="springgreen3")+
        geom_text(aes(label=B), vjust=1.6, color="white", size=5)+
          ggtitle("degree of each node in community 6") +
          xlab("id") + ylab("degree") +
          theme(axis.title = element_text(size = 15)) +
          theme(axis.text = element_text(size = 15))  +
          theme(plot.title = element_text(size =15)) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
     
      df.edges2 <- do.call(rbind, Map(data.frame, A=ld2, B=l2))
      ggplot(df.edges2, aes(x=factor(A), y=B)) +
        geom_bar(stat="identity", fill="steelblue3")+
        geom_text(aes(label=B), vjust=1.6, color="white", size=5)+
        ggtitle("degree of each node in community 2") +
        xlab("id") + ylab("degree") +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15))  +
        theme(plot.title = element_text(size =15)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      

    
}

plot_diff_graph <- function(network)
{
  # https://kateto.net/sunbelt2019
  
  
  V(network)$size <- 8
  V(network)$frame.color <- "white"
  #V(network)$color <- "orange"
  V(network)$label.color <- "black" 
  V(network)$label.cex <- 0.8	 
  E(network)$arrow.mode <- 0
  
  
  colrs <- c("lightgray", "tomato", "skyblue", "lightblue", "skyblue", "red")
  for (i in 1:23) {
    V(network)$color[i] <- colrs[3]
    V(network)$shape[i] <- "square"
    V(network)$label.color[i] <- "black"
  }
  
  for (i in 24:55) {
    V(network)$color[i] <- colrs[2]
    V(network)$shape[i] <- "circle"
    if (0)
    {
    if( i == 44)
    {
      V(network)$color[i] <- adjustcolor("tomato", alpha.f = 1)
      V(network)$label.color[i] <- adjustcolor("black", alpha.f = 1)
      V(network)$frame.color[i] <- adjustcolor("white", alpha.f = 1)
      V(network)$size[i] <- 0 
    }
    }
  }
  
  
  lo <- layout_with_kk(network) # create a layout
  lo <- norm_coords(lo, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  plot(network, edge.color="gray50",layout=lo)
  plot(network, layout= layout_with_kk)
  
  plot(network) 
  
  legend('topleft',  c("Community 1", "Community 2","Community 3","Community 4","Community 5","Community 6"), 
         pch=c(0,1,0,1,0,1), title="Communities",
         col=c( "skyblue", "pink", "orange", "lightgreen","red" ,"grey"), 
         pt.bg=c( "skyblue","pink", "orange", "lightgreen","red" ,"grey"), 
         pt.cex=2, cex=.8, bty="n", ncol=1)
  
  #nn <- delete_vertices(network, 44)
  #plot(nn)
  legend(x=-1, y=-1, c("control", "condition"),  
         col=c( "orange", "yellowgreen"), pt.bg=c( "orange", "yellowgreen"), 
         pt.cex=2, cex=.8, bty="n", ncol=1, pch = c(1,2))
  
  exp = 2; infl = 2 ; features = "cluster"
  mcl.res <- mcl(cor_matrix, addLoops = TRUE, expansion =exp, inflation = infl, 
               allow1 = TRUE,max.iter = 100, ESM = FALSE)
  print(mcl.res)

  for (i in 1:23) {
    V(network)$color[i] <- "blue"
    V(network)$label.color[i] <- "white"
    V(network)$shape[i] <- "square"
    if( i == 2)
      V(network)$color[i] <- "deepskyblue"
    else if( i == 14)
    {
      V(network)$color[i] <- "pink"
      V(network)$label.color[i] <- "white"
    }
    else if( i == 18)
    {
      V(network)$color[i] <- "gray50"
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
    V(network)$color[i] <- "green"
    V(network)$label.color[i] <- "black"
    V(network)$shape[i] <- "circle"
    if( i == 44)
    {
      #V(network)$color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$label.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$frame.color[i] <- adjustcolor("white", alpha.f = .0)
      #V(network)$size[i] <- 0 
      V(network)$color[i] <- "yellow"
      V(network)$label.color[i] <- "black"
      
      }
  }
  
    
  lo <- layout_with_kk(network) # create a layout
  lo <- norm_coords(lo, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  #this working . taken for bibm paper
  # orphan nodes are mixed inside, that has to be handled
  plot(network, layout=lo, vertex.label.font = 2)
  
  #plot(network, vertex.label.font = 2)
  legend('topleft',  c("Community 1", "Community 2","Community 3","Community 4","Community 5","Community 6"), 
         pch=c(15,15,15,15,16,16), title="Communities",
         col=c( "deepskyblue", "blue", "pink", "gray50", "gold", "green"), 
         pt.bg=c("deepskyblue", "blue", "pink", "gray50", "gold", "green"), 
         pt.cex=1.6, cex=.8, bty="n")
  
    
  #nn <- delete_vertices(network, 44)
  #plot(nn, layout=lo, vertex.label.font = 2)
  
  
  
}
