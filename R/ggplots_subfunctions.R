## These functions handle plots of somRes objects
## ----------------------------------------------------------------------------
### subfunctions
theme_set(theme_bw(base_size = 12) + 
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1),
              strip.background = element_blank(),
              strip.text = element_text(size=8, lineheight = 6)
            )
)

### Plots (ggplot2 version) grid-like : one graph using parameters$the.grid$coord as coordinates on the plan
#############################################################################################
ggplotGrid<- function(what, type, values, clustering, print.title,
                           the.titles, the.grid, args=NULL){
  
  # Axes labels 
  ################################################
  varname <- args$varname
  if(is.null(args$varname)){
    varname <- colnames(values)[1]
  }
  if(is.null(args$labelcolor)){
    if(is.null(args$sc) | type=="color"){
      if(type=="hitmap" | type=="poly.dist"){
        labelcolor <- "Number of\nobservations"
      } else if(what=="prototypes"){
        labelcolor <- paste0("value of\n", varname, "\nfor each prototype")
      } else {
        labelcolor <- paste0("mean of\n", varname)
      }
    } else {
      labelcolor <- "Super_Clusters"
    }
  } else {
    labelcolor <- args$labelcolor
  }
  
  # Data 
  ################################################
  if(type!="poly.dist"){
    dataplot <- data.frame("varname"=as.matrix(values)[,1], "SOMclustering"=clustering, the.grid$coord[clustering,], "Nb"=1)
  }
  
  # Plot
  ################################################
  if(type=="poly.dist"){
    maxi <- max(unlist(values))
    # Distance on the grid (min distance beween polygons = 1 --> 0.5 for each polygon)
    values <- lapply(values, function(x) 0.429*((maxi-x)/maxi+0.05))
    
    dataplot <- lapply(1:length(values), function(x) coords_polydist(x, values, the.grid))
    dataplot <- data.frame(do.call("rbind", dataplot))
    
    if(is.null(args$sc)){
      #labelcolor <- "Nb_observations"
      datacolor <- data.frame(table(clustering), stringsAsFactors = F)
      colnames(datacolor) <- c("id", "varcolor")
    } else {
      #labelcolor <- "Super_Clusters"
      datacolor <- data.frame("id"=1:length(args$sc), "varcolor" = as.character(args$sc))
    }
    
    dataplot$numrow <- rownames(dataplot)
    dataplot <- merge(dataplot, datacolor, by="id", all.x=T, sort=F)
    dataplot <- dataplot[order(dataplot$numrow),]
    if(is.null(args$sc)){
      dataplot$varcolor <- ifelse(is.na(dataplot$varcolor), 0, dataplot$varcolor)
    }
    
    tp <- ggplot(dataplot, aes(x = x, y = y)) +
      geom_polygon(data=dataplot, aes(fill = varcolor, group = id))
  }
  
  if(type == "hitmap"){
    if(is.null(args$sc)){
      dataplot<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y, length)
      dataplot$varname <- dataplot$Nb
    } else {
      dataplot<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y + varname, length)
      dataplot$varname <- as.factor(dataplot$varname)
    }
    
    if(is.null(args$maxsize)){
      maxsize <- max(dataplot$Nb)
      if(maxsize>25){
        maxsize <- 25
      }
    } else maxsize <- args$maxsize
    if(is.null(args$minsize)){
      minsize <- min(dataplot$Nb)
      if(minsize>25){
        minsize <- 5
      }
    } else maxsize <- args$maxsize
    
    # # if(is.null(args$minsize)) minsize <- min(dataplot$Nb) else minsize <- args$minsize
    # if(is.null(args$maxsize)) maxsize <- 25 else maxsize <- args$maxsize
    # if(is.null(args$minsize)) minsize <- 1 else minsize <- args$minsize
    tp <- ggplot(dataplot, aes(x=x, y=y)) + 
      geom_point(aes(size = Nb, fill=varname), pch = 21, show.legend = T) +
      scale_size_continuous(range=c(1,maxsize), 
                            breaks = unique(c(min(dataplot$Nb), 
                                              floor(median(dataplot$Nb)), 
                                              max(dataplot$Nb)))) +
      labs(size="Number of\nobservations")
  }
  
  if(type == "color"){
    dataplot <- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)
    if(the.grid$topo == "square"){
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=varname)) + 
        geom_bin2d(stat="identity", linetype=1, color="grey")
    } else {
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=varname)) + 
        geom_hex(stat="identity", linetype=1, color="grey") 
    }
  }
  
  if(type=="grid"){
    dataplot <- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)
    if(the.grid$topo == "square"){
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=factor(varname))) + 
        geom_bin2d(stat="identity", linetype=1, color="grey", size=0.6)
    } else {
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=factor(varname), group=1)) + 
        geom_hex(stat="identity", linetype=1, color="grey", size=0.6)
    }
  }

  tp <- tp +  ggtitle(myTitle(args, what)) + coord_fixed() + 
    xlim(0.5, max(dataplot$x)+0.5) + ylim(0.5, max(dataplot$y)+0.5) + 
    labs(fill = labelcolor) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  if(print.title==T){
    datagrid <- data.frame(the.grid$coord, the.titles)
    tp <- tp + geom_text(data=datagrid, aes(x=x, y=y, label=the.titles, fill=NULL))
  }
  
  tp
}

### Plots (ggplot2 version) using facet_wraps : need to have one graph by cluster
#############################################################################################

ggplotFacet <- function(what, type, values, clustering=NULL, print.title,
                      the.titles, is.scaled, the.grid, args){
  ordered.index <- orderIndexes(the.grid, type)

  # Axes labels 
  ################################################
  vary <- "values"
  if(!(type %in% c("names", "words", "pie")))  {
    if(is.scaled==T){
      values <- scale(values, is.scaled, is.scaled)
      vary <- "scaled_values"
    }
  } 
  
  labely <- vary
  if(what %in% c("obs", "add") & type %in% c("radar", "barplot", "lines")){
    labely <- paste0("mean of ", labely)
  }
  if(what==" prototypes"){
    labely <- "values for each prototype"
  }
  if(type=="names"){
    labely <- "frequency of values"
    if(type=="names" & is.null(args$varname)==F){
      labely <- paste("frequency of", args$varname, "values")
    }
  }

  # Data (ggplot way)
  ################################################
  dataplot <- data.frame(values)
  dataplot$ind <- rownames(dataplot)
 # dataplot$SOMclustering <- clustering
  dataplot$SOMclustering <- factor(clustering, levels=ordered.index)
  if(is.null(args$sc)){
    dataplot <- melt(dataplot,  measure.vars = colnames(data.frame(values)), value.name = vary)
    dataplot$variable <- as.factor(dataplot$variable)
    labelcolor <- "variable"
  } else {
    dataplot$varcolor <- args$sc
    dataplot <- melt(dataplot,  measure.vars = colnames(data.frame(values)), value.name = vary)
    dataplot$varcolor <- as.factor(dataplot$varcolor)
    labelcolor <- "Super_Clusters"
    colnames(dataplot)[match("varcolor", colnames(dataplot))] <- labelcolor
  }
  
  # Plot
  ################################################
  if(type == "barplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_bar(stat='summary', fun=mean) + ylab(labely) 
  }
  if(type == "boxplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_boxplot() 
  }
  if(type == "lines"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, group=1, colour=labelcolor)) +
      geom_point(stat='summary', fun=mean) + ylab(labely) 
    if(is.null(args$sc)){
      tp <- tp + stat_summary(fun=mean, geom="line", colour="black")
    } else {
      tp <- tp + stat_summary(fun=mean, geom="line", mapping = aes_string(colour=labelcolor), show.legend = F)
    }
  }
  if(type == "names"){
     dataplot$nb <- 1
     dataplot <- aggregate(data=dataplot, nb ~ values + SOMclustering, length)
     tp <- ggplot(dataplot, aes(label = values, size=nb)) +
      geom_text_wordcloud(stat="identity", alpha=0.7) + labs(subtitle = labely)
  }
  if(type == "words"){
    dataplot <- aggregate(data=dataplot, values ~ variable + SOMclustering, sum)
    tp <- ggplot(dataplot, aes(label = variable, size=values)) +
      geom_text_wordcloud(stat="identity", alpha=0.7) + labs(subtitle = "sum of values by variable")
  }
  if(type == "pie"){
    dataplot$Nb <- 1
    dataplot$Nbcluster <- 1
    datatot <- aggregate(data=dataplot, Nbcluster ~ SOMclustering, sum)
    dataplot <- aggregate(data=dataplot, Nb ~ SOMclustering + values, sum)
    dataplot <- merge(dataplot, datatot, by="SOMclustering", all.x=T)
    dataplot$Share <- dataplot$Nb/dataplot$Nbcluster
    
    tp <-  ggplot(dataplot, aes(x=Nbcluster/2, y=Share, fill=values, width=Nbcluster)) +
              geom_bar(position = "fill", stat="identity") + 
              coord_polar("y") + 
              theme(axis.text.x = element_blank()) + 
              ylab(NULL) + xlab("Number of individuals in the cluster") + 
              guides(fill=guide_legend(title=labelcolor))
  }
  # Handling of the grid order
  mylabels <- the.titles[ordered.index]
  names(mylabels) <- levels(dataplot$SOMclustering)
  
  tp <- tp + facet_wrap(SOMclustering ~ ., 
                        drop=FALSE, 
                        nrow=the.grid$dim[2],
                        labeller=labeller(SOMclustering = mylabels),
                        dir = "h") +
             ggtitle(myTitle(args, what)) 

  # Clusters titles
  if(print.title==F){
    tp <- tp + theme(strip.background = element_blank(),
                     strip.text = element_blank())
  }
  return(tp)
}

ggplotPolydist <- function(values, clustering, print.title,
                           the.titles, the.grid, args=NULL){
  maxi <- max(unlist(values))
  # Distance on the grid (min distance beween polygons = 1 --> 0.5 for each polygon)
  values <- lapply(values, function(x) 0.429*((maxi-x)/maxi+0.05))
  
  coords_dist <- lapply(1:length(values), function(x) coords_polydist(x, values, the.grid))
  coords_dist <- data.frame(do.call("rbind", coords_dist))
  
  if(is.null(args$sc)){
    labelcolor <- "Nb_observations"
    datacolor <- data.frame(table(clustering), stringsAsFactors = F)
    colnames(datacolor) <- c("id", "varcolor")
  } else {
    labelcolor <- "Super_Clusters"
    datacolor <- data.frame("id"=1:length(args$sc), "varcolor" = as.character(args$sc))
  }
  
  coords_dist$numrow <- rownames(coords_dist)
  coords_dist <- merge(coords_dist, datacolor, by="id", all.x=T, sort=F)
  coords_dist <- coords_dist[order(coords_dist$numrow),]
  if(is.null(args$sc)){
    coords_dist$varcolor <- ifelse(is.na(coords_dist$varcolor), 0, coords_dist$varcolor)
  }
  
  p <- ggplot(coords_dist, aes(x = x, y = y)) +
    geom_polygon(data=coords_dist, aes(fill = varcolor, group = id)) + 
    coord_fixed()  + 
    guides(fill=guide_legend(title=labelcolor))
  if(print.title==T){
    datagrid <- data.frame(the.grid$coord, the.titles)
    p <- p + geom_text(data=datagrid, aes(x=x, y=y, label=the.titles, fill=NULL))
  }
  p
}


ggplotEnergy <- function(sommap) {
  # possible only if some intermediate backups have been done
  if (is.null(sommap$backup)) {
    stop("no intermediate backups have been registered\n", call.=TRUE)
  } else {
    dataenergy <- data.frame("Steps" = sommap$backup$steps, "Energy" = sommap$backup$energy)
    p <- ggplot(dataenergy, aes(x=Steps, y=Energy)) + 
      geom_line() + geom_point() + 
      ggtitle("Energy evolution") 
    p
  }
}
