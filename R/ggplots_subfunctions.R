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
                           the.titles, is.scaled=F, the.grid, args=NULL, variable=1, labelcolor=NULL){
  
  # Colors
  ################################################
   ncolors <- prod(the.grid$dim)
   if (!is.null(args$col) && length(args$col)!=2) {
     warning("Incorrect number of colors (2 expected). Arbitrary color will be used\n",
             call.=TRUE, immediate.=TRUE)
     args$col <- NULL
   }
   colorsvars <- args$col
   if (is.null(args$col)) {
     colorsvars <- c("#49AAFF", "#3F007D")
   }
  
  # Axes labels 
  ################################################
  if(is.null(labelcolor)){
    if(type=="hitmap"){
      labelcolor <- "Number of\nobservations"
    } else if(what=="prototypes"){
      labelcolor <- paste0("value of\n", variable, "\nfor each prototype")
    } else {
      labelcolor <- paste0("mean of\n", variable)
    }
  }


  # Data 
  ################################################
  dataplot <- data.frame("varname"=as.matrix(values)[,1], "SOMclustering"=clustering, the.grid$coord[clustering,], "Nb"=1)
  
  # Plot
  ################################################
  if(type == "hitmap"){
    dataplot<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y, length)
    nbtot <- length(clustering)
    tp <- ggplot(dataplot, aes(x=x, y=y)) + 
      geom_point(aes(size = Nb, fill=Nb), pch = 21, show.legend = T) +
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
      # tp <- ggplot(datavar, aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5)) + 
      #   geom_rect(data=datavar, aes(fill=varname)) + labs(fill = labelcolor)
    } else {
      tp <- ggplot(dataplot, aes(x=x, y=y, fill=varname)) + 
        geom_hex(stat="identity", linetype=1, color="grey") 
    }
  }
  
  tp <- tp + scale_fill_gradient(low = colorsvars[1], high = colorsvars[2]) +
    ggtitle(myTitle(args, what)) + coord_fixed() + 
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
                      the.titles, is.scaled, the.grid, args, labely=NULL){
  ordered.index <- orderIndexes(the.grid, type)
  # Colors
  ################################################
 if(type == "names" | type == "words"){
    # 1 color used
    ncolors <- 2
  } else if(type=="poly.dist"){
    ncolors <- length(values)
  } else {
    # Nb of variable, discrete colors
    ncolors <- ncol(as.matrix(values))
  }
  
  if (!is.null(args$col) && length(args$col)>1 &&
      length(args$col)!=ncolors) {
    warning("unadequate number of colors; arbitrary color will be used\n",
            call.=TRUE, immediate.=TRUE)
    args$col <- NULL
  }
  colorsvars <- args$col
  if (is.null(args$col)) {
    if(type %in% c("barplot", "boxplot", "lines", "pie", "radar")){ 
      # Discrete
        if(ncolors<=8){
        colorsvars <- brewer.pal(max(3, ncolors),"Set2")[1:ncolors]
      } else { 
        set.seed(123)
        colorsvars <- rainbow(ncolors)
      }
    } else {
      #continuous
      colorsvars <- rev(brewer.pal(9,"Purples")[2:9])
    }
  } 
  if (length(args$col)==1) {
    colorsvars <- rep(args$col, ncolors)
  }
  
  # Axes labels 
  ################################################
  if(is.null(labely)){
    labely <- "values"
    if(!(type %in% c("names", "words")) & is.scaled==T)  {
      values <- scale(values, is.scaled, is.scaled)
      labely <- "scaled_values"
    }
  }

  
  # Data (ggplot way)
  ################################################
  if(type=="poly.dist"){
    dataplot <- melt(values, value.name="poly.dist")
    colnames(dataplot)[ncol(dataplot)] <- "SOMclustering"
    dataplot$ind <- rownames(dataplot)
  } else {
    dataplot <- data.frame(values)
    dataplot$ind <- rownames(dataplot)
    dataplot$SOMclustering <- clustering
    dataplot <- melt(dataplot,  measure.vars = colnames(data.frame(values)), value.name = labely)
  }
  
  # Plot
  ################################################
  if(type == "barplot" | type == "radar"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = labely, fill='variable')) +
      geom_bar(stat='summary', fun.y=mean) + ylab(paste("mean of", labely)) +
      scale_fill_manual(values=colorsvars)
  }
  if(type == "radar"){
    tp <- tp + coord_polar() +  theme(axis.text.x=element_blank())
  }
  if(type == "boxplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = labely, fill='variable')) +
      geom_boxplot() + ylab(paste("mean of", labely)) +
      scale_fill_manual(values=colorsvars)
  }
  if(type == "lines"){
    col_line <- "black" 
    if(length(args$col)==1) col_line <- args$col
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = labely, group=1, colour='variable')) +
      geom_point(stat='summary', fun.y=mean) + 
      stat_summary(fun.y=mean, geom="line", colour=col_line) + 
      scale_colour_manual(values = colorsvars)
  }
  if(type == "names"){
    tp <- ggplot(dataplot, aes(label = ind)) +
      geom_text_wordcloud(stat="unique", colour=colorsvars[1], alpha=0.7) +
      scale_size_area(max_size = 13) 
  }
  if(type == "poly.dist"){
    tp <- ggplot(dataplot, aes(x=x, y=y)) + 
      geom_polygon(aes(fill=stat(count))) +
      labs(fill = labely)
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
  tp
}
