## These functions handle plots of somRes objects
## ----------------------------------------------------------------------------
### subfunctions
theme_set(theme_bw(base_size = 1) + 
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1),
              strip.background = element_blank(),
              strip.text = element_text(size=8, lineheight = 6)
            )
)


ggplotObs <- function(what, type, values, clustering=NULL, print.title,
                           the.titles, is.scaled, the.grid, args){
  ordered.index <- orderIndexes(the.grid, type)
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
      colorsvars <- brewer.pal(max(3, ncolors),"Set2")[1:ncolors]
    } else {
      set.seed(123)
      colorsvars <- sample(rainbow(ncolors, ncolors)) 
    }
  } 
  if (length(args$col)==1) {
    colorsvars <- rep(args$col, ncolors)
  }
  labely <- "values"
  if(type=="numeric" & is.scaled==T)  {
    values <- scale(values, is.scaled, is.scaled)
    labely <- "scaled_values"
  }

  dataplot <- data.frame(values)
  dataplot$ind <- rownames(dataplot)
  dataplot$SOMclustering <- clustering
 
  dataplot <- melt(dataplot,  measure.vars = colnames(data.frame(values)), value.name = labely)
  
  if(type == "barplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = labely, fill='variable')) +
      geom_bar(stat='summary', fun.y=mean) +
      scale_fill_manual(values=colorsvars)
  }
  if(type == "boxplot"){
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = labely, fill='variable')) +
      geom_boxplot() +
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
  if(type == "hitmap"){
    dataplot <- data.frame(table(dataplot$SOMclustering))
    colnames(dataplot) <- c("SOMclustering", "Nb")
    tp <- ggplot(dataplot, aes(x = 1, y = 1)) +
      #geom_rect(aes(xmin=as.numeric(Var1)-0.5*abs(value),
      #              xmax=as.numeric(Var1)+0.5*abs(value),
      #              ymin=as.numeric(Var2)-0.5*abs(value),
      #              ymax=as.numeric(Var2)+0.5*abs(value),fill=colorsvars[1]) +
      geom_count(colour=colorsvars[1], alpha=0.7, shape=15) +
      scale_size_continuous(range=c(5, 10)) 
#      scale_size_area(max_size = 0.45) #+
      #theme(axis.title.x=element_blank(),
      #      axis.text.x=element_blank(),
      #      axis.ticks.x=element_blank(),
      #      axis.title.y=element_blank(),
      #      axis.text.y=element_blank(),
      #      axis.ticks.y=element_blank())
      
  }
  # Handling of the grid order
  tp <- tp + facet_wrap(factor(SOMclustering, levels=ordered.index, labels=the.titles[ordered.index]) ~ ., 
                        drop=FALSE, 
                        nrow=the.grid$dim[1],
                        dir = "h")
  # Clusters titles
  if(print.title==F){
    tp <- tp + theme(strip.background = element_blank(),
                     strip.text = element_blank())
  }
  tp
}

