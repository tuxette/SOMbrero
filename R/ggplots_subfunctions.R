## These functions handle plots of somRes objects
## ----------------------------------------------------------------------------
### subfunctions
theme_set(theme_bw(base_size = 8) + 
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
                           the.titles, is.scaled=F, the.grid, args=NULL, variable){
  
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
   if(what=="prototypes"){
     labelcolor <- paste0("value of\n", variable, "\nfor each prototype")
   } else {
     labelcolor <- paste0("mean of\n", variable)
   }

  # Data 
  ################################################
  dataplot <- data.frame("varname"=as.matrix(values)[,1], "SOMclustering"=clustering, the.grid$coord[clustering,], "Nb"=1)
  datanb<- aggregate(data=dataplot, Nb ~ SOMclustering + x + y, length)
  datavar<- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)

  # Plot
  ################################################
  if(type == "hitmap"){
    nbtot <- length(clustering)
    tp <- ggplot(datanb, aes(x=x, y=y)) + 
      geom_point(aes(size = Nb, fill=Nb), pch = 21, show.legend = T) +
      scale_size_continuous(range=c(1,30), 
                            breaks = unique(c(min(datanb$Nb), floor(median(datanb$Nb)), max(datanb$Nb)))) 
    # Le rayon des cercles max est de 0.5
    # geom_circle(aes(x0 = x, y0 = y, r = Nb/nbtot, fill = Nb), show.legend = T) +
  }
  if(type == "color"){
    # Le rayon des cercles max est de 0.5
    tp <- ggplot(datanb, aes(xmin=x-0.5, xmax=x+0.5, ymin=y-0.5, ymax=y+0.5)) + 
      geom_rect(data=datavar, aes(fill=varname)) + labs(fill = labelcolor)
  }
   if(print.title==T){
     datagrid <- data.frame(the.grid$coord, the.titles)
     tp <- tp + geom_text(data=datagrid, aes(x=x, y=y, label=the.titles ))
   }
  
  tp + xlim(0.4, 5.6) + ylim(0.4,5.6) +
    scale_fill_gradient(low = colorsvars[1], high = colorsvars[2]) +
    ggtitle(myTitle(args, what)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

### Plots (ggplot2 version) using facet_wraps : need to have one graph by cluster
#############################################################################################

ggplotFacet <- function(what, type, values, clustering=NULL, print.title,
                      the.titles, is.scaled, the.grid, args){
  ordered.index <- orderIndexes(the.grid, type)

  # Colors
  ################################################
  ncolors <- ncol(as.matrix(values))
  if (!is.null(args$col) && length(args$col)>1 &&
      length(args$col)!=ncolors) {
    warning("unadequate number of colors; arbitrary color will be used\n",
            call.=TRUE, immediate.=TRUE)
    args$col <- NULL
  }
  colorsvars <- args$col
  if (is.null(args$col)) {
    if(ncolors<=8){
      # Discrete
      if(type %in% c("barplot", "boxplot", "lines", "pie", "radar")){ 
        colorsvars <- brewer.pal(max(3, ncolors),"Set2")[1:ncolors]
      } else { #continuous
        colorsvars <- rev(brewer.pal(9,"Purples")[2:9])
      }
    } else {
      set.seed(123)
      colorsvars <- rainbow(ncolors)
    }
  } 
  if (length(args$col)==1) {
    colorsvars <- rep(args$col, ncolors)
  }
  
  # Axes labels 
  ################################################
  labely <- "values"
  if(type=="numeric" & is.scaled==T)  {
    values <- scale(values, is.scaled, is.scaled)
    labely <- "scaled_values"
  }
  
  # Data (ggplot way)
  ################################################
  dataplot <- data.frame(values)
  dataplot$ind <- rownames(dataplot)
  dataplot$SOMclustering <- clustering
  
  dataplot <- melt(dataplot,  measure.vars = colnames(data.frame(values)), value.name = labely)
  
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
  # Handling of the grid order
  tp <- tp + facet_wrap(factor(SOMclustering, levels=ordered.index, labels=the.titles[ordered.index]) ~ ., 
                        drop=FALSE, 
                        nrow=the.grid$dim[1],
                        dir = "h") +
             ggtitle(myTitle(args, what)) 
  
  # Clusters titles
  if(print.title==F){
    tp <- tp + theme(strip.background = element_blank(),
                     strip.text = element_blank())
  }
  tp
}
