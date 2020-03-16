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
                           the.titles, is.scaled=F, the.grid, args=NULL, variable=1){
  
  # Axes labels 
  ################################################
  
  if(is.null(args$labelcolor)){
    if(is.null(args$sc) | type=="color"){
      if(type=="hitmap"){
        labelcolor <- "Number of\nobservations"
      } else if(what=="prototypes"){
        labelcolor <- paste0("value of\n", variable, "\nfor each prototype")
      } else {
        labelcolor <- paste0("mean of\n", variable)
      }
    } else {
      labelcolor <- "Super_Clusters"
    }
  } else {
    labelcolor <- args$labelcolor
  }
  
  # Data 
  ################################################
  dataplot <- data.frame("varname"=as.matrix(values)[,1], "SOMclustering"=clustering, the.grid$coord[clustering,], "Nb"=1)
  
  # Plot
  ################################################

  if(type == "hitmap"){
    if(is.null(args$sc)){
      dataplot<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y, length)
      dataplot$varname <- dataplot$Nb
    } else {
      dataplot<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y + varname, length)
      dataplot$varname <- as.factor(dataplot$varname)
    }
    tp <- ggplot(dataplot, aes(x=x, y=y)) + 
      geom_point(aes(size = Nb, fill=varname), pch = 21, show.legend = T) +
      scale_size_continuous(range=c(1,30), 
                            breaks = unique(c(min(dataplot$Nb), 
                                              floor(median(dataplot$Nb)), 
                                              max(dataplot$Nb)))) +
      labs(size="Number of\nobservations")
  }
  if(type == "color"){
    dataplot<- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)
    if(args$topo == "square"){
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=varname)) + 
        geom_bin2d(stat="identity", linetype=1, color="grey")
    } else {
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=varname)) + 
        geom_hex(stat="identity", linetype=1, color="grey") 
    }
  }
  
  if(type=="grid"){
    dataplot<- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)
    if(args$topo == "square"){
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=factor(varname))) + 
        geom_bin2d(stat="identity", linetype=1, color="grey")
    } else {
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=factor(varname))) + 
        geom_hex(stat="binhex", position="identity", linetype=1, color="grey") 
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
  if(!(type %in% c("names", "words")))  {
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
    if(type=="names" & is.null(args$variable)==F){
      labely <- paste("frequency of", args$variable, "values")
    }
  }
  


  # Data (ggplot way)
  ################################################
  dataplot <- data.frame(values)
  dataplot$ind <- rownames(dataplot)
  dataplot$SOMclustering <- clustering
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
      geom_bar(stat='summary', fun.y=mean) + ylab(labely) 
  }
  if(type == "boxplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_boxplot() 
  }
  if(type == "lines"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, group=1, colour=labelcolor)) +
      geom_point(stat='summary', fun.y=mean) + ylab(labely) 
    if(is.null(args$sc)){
      tp <- tp + stat_summary(fun.y=mean, geom="line", colour="black")
    } else {
      tp <- tp + stat_summary(fun.y=mean, geom="line", mapping = aes_string(colour=labelcolor), show.legend = F)
      print("tt")
    }
  }
  if(type == "names"){
     tp <- ggplot(dataplot, aes(label = values)) +
      geom_text_wordcloud(stat="identity", alpha=0.7) + labs(subtitle = labely)
  }
  if(type == "words"){
    dataplot <- aggregate(data=dataplot, values ~ variable + SOMclustering, sum)
    tp <- ggplot(dataplot, aes(label = variable, size=values)) +
      geom_text_wordcloud(stat="identity", alpha=0.7, area_corr=TRUE) + labs(subtitle = "sum of values by variable")
  }
  if(type == "poly.dist"){
    tp <- ggplot(dataplot, aes(x=x, y=y)) + 
      geom_polygon(aes(fill=stat(count))) +
      labs(fill = labely)
  }
  if(type == "pie"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_boxplot() 
  }
  # Handling of the grid order
  tp <- tp + facet_wrap(factor(SOMclustering, levels=ordered.index, labels=the.titles[ordered.index]) ~ ., 
                        drop=FALSE, 
                        nrow=the.grid$dim[2],
                        dir = "h") +
             ggtitle(myTitle(args, what)) 

  # Clusters titles
  if(print.title==F){
    tp <- tp + theme(strip.background = element_blank(),
                     strip.text = element_blank())
  }
  return(tp)
}
