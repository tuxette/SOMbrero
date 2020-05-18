## These functions handle plots of somRes objects
## ----------------------------------------------------------------------------
theme_facet<- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size=8, lineheight = 6),
        panel.border = element_rect(fill = NA, colour = "grey"),
        panel.spacing = unit(0,'lines')
      )
}

### Plots (ggplot2 version) grid-like : one graph using parameters$the.grid$coord as coordinates on the plan
#############################################################################################
ggplotGrid <- function(what, type, values, clustering, show.names,
                       names, the.grid, args = NULL) {
  
  # Axes labels 
  ################################################
  if(is.null(args$varname)) {
    varname <- colnames(values)[1] 
  } else varname <- args$varname

  labelcolor <- ""
  if (is.null(args$labelcolor)) {
    if (is.null(args$sc) | type=="color") {
      if (type=="hitmap" | type=="poly.dist") {
        labelcolor <- "Number of\nobservations"
      } else if (what=="prototypes") {
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
  if (type != "poly.dist") {
    dataplot <- data.frame("varname" = as.matrix(values)[,1],
                           "SOMclustering" = clustering, 
                           the.grid$coord[clustering,],
                           "Nb" = 1)
  }
  
  # Plot
  ################################################
  if (type == "poly.dist") {
    maxi <- max(unlist(values))
    mini <- min(unlist(values))
    # reverse scale (big distance = small proximity)
    values <- lapply(values, function(x) (maxi-x)/(maxi-mini))
    # Project on [0.05 - 0.45] interval
    values <- lapply(values, function(x) x*(0.45-0.05) + 0.05)
    #values <- lapply(values, function(x) 0.429 * ((maxi-x)/maxi+0.05))
    
    dataplot <- lapply(1:length(values), function(x) 
      coords_polydist(x, values, the.grid))
    dataplot <- data.frame(do.call("rbind", dataplot))
    
    if (is.null(args$sc)) {
      datacolor <- data.frame(table(clustering), stringsAsFactors = FALSE)
      colnames(datacolor) <- c("id", "varcolor")
    } else {
      datacolor <- data.frame("id" = 1:length(args$sc), 
                              "varcolor" = as.character(args$sc))
    }
    
    dataplot$numrow <- rownames(dataplot)
    dataplot <- merge(dataplot, datacolor, by="id", all.x = TRUE, sort = FALSE)
    dataplot <- dataplot[order(dataplot$numrow), ]
    if (is.null(args$sc)) {
      dataplot$varcolor <- ifelse(is.na(dataplot$varcolor), 0, dataplot$varcolor)
    }
    
    tp <- ggplot(dataplot, aes_string(x = "x", y = "y")) +
      geom_polygon(data=dataplot, aes_string(fill = "varcolor", group = "id"))
  }
  
  if (type == "hitmap") {
    if (is.null(args$sc)) {
      dataplot <- aggregate(data=dataplot, Nb ~ SOMclustering + x + y, length)
      dataplot$varname <- dataplot$Nb
    } else {
      dataplot <- aggregate(data=dataplot, Nb ~ SOMclustering + x + y + varname, length)
      dataplot$varname <- as.factor(dataplot$varname)
    }
    
    tp <- ggplot(dataplot, aes_string(x = "x", y = "y")) + 
      geom_point(aes_string(size = "Nb", color = "varname")) +
      scale_size_area(breaks=c(min(dataplot$Nb), median(dataplot$Nb), max(dataplot$Nb)),
                                   max_size=min(25,max(dataplot$Nb)))+
      labs(size = "Number of\nobservations") + labs(color=labelcolor)
  }
  
  if (type == "color") {
    dataplot <- aggregate(data=dataplot, varname ~ SOMclustering + x + y, mean)
    if (the.grid$topo == "square") {
      tp <- ggplot(dataplot, aes_string(x = "x", y = "y", fill = "varname")) + 
        geom_bin2d(stat = "identity", linetype = 1, color = "grey")
    } else {
      if (requireNamespace("hexbin", quietly = TRUE)) {
        tp <- ggplot(dataplot, aes_string(x = "x", y = "y", fill = "varname")) + 
          geom_hex(stat = "identity", linetype = 1, color = "grey")
      } else {
        stop("'hexbin' package required for this plot.", call. = TRUE)
      }
    }
  }
  
  if (type == "grid") {
    dataplot <- aggregate(data = dataplot, varname ~ SOMclustering + x + y, mean)
    if (the.grid$topo == "square") {
      dataplot$varname <- factor(dataplot$varname)
      tp <- ggplot(dataplot, aes_string(x = "x", y = "y", fill = "varname")) + 
        geom_bin2d(stat = "identity", linetype = 1, color = "grey", size = 0.6)
    } else {
      dataplot$varname <- factor(dataplot$varname)
      if (requireNamespace("hexbin", quietly = TRUE)) {
        tp <- ggplot(dataplot, aes_string(x = "x", y = "y", fill = "varname",
                                          group = "1")) + 
          geom_hex(stat = "identity", linetype = 1, color = "grey", size = 0.6)
      } else {
        stop("'hexbin' package required for this plot.", call. = TRUE)
      }
    }
  }

  if(type != "poly.dist"){
    tp <- tp + xlim(0.5, max(dataplot$x) + 0.5) + ylim(0.5, max(dataplot$y) + 0.5)
  }
  
  tp <- tp +  ggtitle(myTitle(args, what)) + coord_fixed() + 
    labs(fill = labelcolor) + theme_void() + theme(panel.border=element_rect(fill = NA, colour = "grey"))

  if (show.names) {
    datagrid <- data.frame(the.grid$coord, names)
    tp <- tp + geom_text(data = datagrid, 
                         aes_string(x = "x", y = "y", label = "names", fill = NULL))
  }
  
  tp
}

### Plots (ggplot2 version) using facet_wraps : need to have one graph by cluster
#############################################################################################

ggplotFacet <- function(what, type, values, clustering=NULL, show.names,
                        names, is.scaled, the.grid, args){
  ordered.index <- orderIndexes(the.grid, type)
  # Axes labels 
  ################################################
  vary <- "values"
  if (!(type %in% c("names", "words", "pie"))) {
    if (is.scaled) {
      values <- scale(values, is.scaled, is.scaled)
      vary <- "scaled_values"
    }
  } 
  
  labely <- vary
  if (what %in% c("obs", "add") & type %in% c("barplot", "meanline")) {
    labely <- paste0("mean of ", labely)
  }
  if (what == "prototypes") {
    #if(type == "lines") type <- "meanline"
    labely <- "values for each prototype"
  }
  if (type == "names") {
    labely <- "frequency of values"
    if(type == "names" & !is.null(args$varname)) {
      labely <- paste("frequency of", args$varname, "values")
    }    
    if(type == "names" & args$varname %in% c("row.names", "names")) {
      labely <- paste("repartition of", args$varname, "values")
    }
  }

  # Data (ggplot way)
  ################################################
  dataplot <- data.frame(values)
  nbvar <- ncol(dataplot)
  colnames(dataplot)[1:nbvar] <- paste0(vary, "-", colnames(dataplot)[1:nbvar])
  dataplot$ind <- rownames(dataplot)
  dataplot$SOMclustering <- factor(clustering, levels=ordered.index)
  
  if(is.null(args$sc)){
    dataplot <- reshape(dataplot,  varying = 1:nbvar, idvar=c("ind", "SOMclustering"), 
                        sep="-", direction="long", timevar="variable")
    dataplot$variable <- as.factor(dataplot$variable)
    labelcolor <- "variable"
  } else {
    dataplot$varcolor <- args$sc
    dataplot <- reshape(dataplot,  varying = 1:nbvar, idvar=c("ind", "SOMclustering", "varcolor"), 
                        sep="-", direction="long", timevar="variable")
    dataplot$varcolor <- as.factor(dataplot$varcolor)
    labelcolor <- "Super_Clusters"
    colnames(dataplot)[match("varcolor", colnames(dataplot))] <- labelcolor
  }
  
  # Plot
  ################################################
  if (type == "barplot") {
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_bar(stat='summary', fun=mean) + ylab(labely) 
  }
  if (type == "boxplot") {
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, fill=labelcolor)) +
      geom_boxplot() 
  }
  if (type == "lines") {
    if (is.null(args$sc)) {
      tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, group='ind')) +
        geom_line(alpha=0.8) + ylab(labely) 
    } else {
      tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, group='ind', color=labelcolor)) +
        geom_line(alpha=0.8) + ylab(labely) 
    }
  } 
  if (type == "meanline") {
    tp <- ggplot(dataplot, aes_string(x = 'variable', y = vary, group=1,
                                      colour=labelcolor)) +
      geom_point(stat='summary', fun=mean) + ylab(labely)
    if (is.null(args$sc)) {
      tp <- tp + stat_summary(fun=mean, geom="line", colour="black")
    } else {
      tp <- tp + stat_summary(fun=mean, geom="line",
                              mapping = aes_string(colour=labelcolor),
                              show.legend = FALSE)
    }
  }
  if (type == "names") {
     dataplot$nb <- 1
     dataplot <- aggregate(data=dataplot, nb ~ values + SOMclustering, length)
     tp <- ggplot(dataplot, aes_string(label = "values", size = "nb")) +
      geom_text_wordcloud(stat="identity", alpha=0.7) + labs(subtitle = labely)
  }
  if(type == "words"){
    dataplot <- aggregate(data=dataplot, values ~ variable + SOMclustering, sum)
    tp <- ggplot(dataplot, aes_string(label = "variable", size = "values")) +
      geom_text_wordcloud(stat="identity", alpha=0.7) + 
      labs(subtitle = "sum of values by variable")
  }
  if(type == "pie"){
    if(is.null(args$varname)==F) labelcolor <- args$varname
    dataplot$Nb <- 1
    dataplot$Nbcluster <- 1
    datatot <- aggregate(data=dataplot, Nbcluster ~ SOMclustering, sum)
    dataplot <- aggregate(data=dataplot, Nb ~ SOMclustering + values, sum)
    dataplot <- merge(dataplot, datatot, by="SOMclustering", all.x=TRUE)
    labely <- "Number of observations in the cluster"
    dataplot$Share <- dataplot$Nb/dataplot$Nbcluster
    # change scale to have proportional areas
    dataplot$Nbcluster <- sqrt(dataplot$Nbcluster)/pi
    if(args$proportional==F){
        labely <- ""
        dataplot$Nbcluster <- max(dataplot$Nbcluster)
    }
    dataplot$halfNbcluster <- dataplot$Nbcluster/2
    tp <-  ggplot(dataplot, aes_string(x="halfNbcluster", y="Share", 
                                       fill="values", width="Nbcluster")) +
              geom_bar(position = "fill", stat="identity") + 
              coord_polar("y") + 
              ylab(NULL) + xlab("Number of individuals in the cluster") + 
              guides(fill=guide_legend(title=labelcolor))
  } 
  # Handling of the grid order
  mylabels <- names[ordered.index]
  names(mylabels) <- levels(dataplot$SOMclustering)
  
  tp <- tp + facet_wrap(SOMclustering ~ ., 
                        drop=FALSE, 
                        nrow=the.grid$dim[2],
                        labeller=labeller(SOMclustering = mylabels),
                        dir = "h") +
             ggtitle(myTitle(args, what)) +
             theme_facet()

  if(type == "pie"){
   tp <- tp + theme(axis.text.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())
  } 
  
  # Clusters titles
  if(show.names==F){
    tp <- tp + theme(strip.text = element_blank())
  }
  return(tp)
}


ggplotEnergy <- function(sommap) {
  # possible only if some intermediate backups have been done
  if (is.null(sommap$backup)) {
    stop("no intermediate backups have been registered\n", call.=TRUE)
  } else {
    dataenergy <- data.frame("Steps" = sommap$backup$steps, 
                             "Energy" = sommap$backup$energy)
    p <- ggplot(dataenergy, aes_string(x = "Steps", y = "Energy")) + 
      geom_line() + geom_point() + ggtitle("Energy evolution") +
      theme_bw() 
    p
  }
}
