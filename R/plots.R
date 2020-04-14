orderIndexes <- function(the.grid, type) {
  # Order of cluster to place them correctly on the grid
  # Used in plots
  match(paste(rep(1:the.grid$dim[1],the.grid$dim[2]),
              rep(the.grid$dim[2]:1, each=the.grid$dim[1]),
              sep="-"),
        paste(rep(1:the.grid$dim[1], each=the.grid$dim[2]),
              rep(1:the.grid$dim[2], the.grid$dim[1]),
              sep="-"))
}


# Produce default colors for non ggplot plots (dendro, dendro3d and igraph.pie)
gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Handle title for plots
myTitle <- function(args, what) {
  if (is.null(args$main)) {
    the.title <- switch(what,
                        "prototypes"="Prototypes overview",
                        "obs"="Observations overview",
                        "add"="Additional variable overview")
  } else the.title <- args$main
  return(the.title)
}

# depth calculation function (adapted from Duncan Murdoch at https://stat.ethz.ch/pipermail/r-help/2005-September/079241.html)
depth3d <- function(x,y,z, pmat, minsize=0.2, maxsize=2) {
  
  # determine depth of each point from xyz and transformation matrix pmat
  tr <- as.matrix(cbind(x, y, z, 1)) %*% pmat
  tr <- tr[,3]/tr[,4]
  
  # scale depth to point sizes between minsize and maxsize
  psize <- ((tr-min(tr) ) * (maxsize-minsize)) / (max(tr)-min(tr)) + minsize
  return(psize)
}

plot3d <- function(x, the.grid, type, variable, args) {
  args$x <- sort(unique(the.grid$coord[,1]))
  args$y <- sort(unique(the.grid$coord[,2]))
  
  if(the.grid$topo=="hexagonal"){
    gridcoordsx <- rep(args$x, length(args$y))
    gridcoordsy <- rep(args$y, each=length(args$x))
    gridvalues<-interp::interp(the.grid$coord[,1], the.grid$coord[,2], x[,variable], # real cordinates
                        xo=gridcoordsx, yo=gridcoordsy, # new coordinates
                        output="points",
                        method="linear")
    gridvalues <- data.frame(gridvalues)
    colnames(gridvalues)[3] <- "z"
    origdata <- data.frame(the.grid$coord, "origz"=x[,variable])
    
    # Replace existing values (for corners)
    gridvalues <- merge(gridvalues, origdata, all.x=T)
    gridvalues$z <- ifelse(is.na(gridvalues$z) & is.na(gridvalues$origz)==F, gridvalues$origz, gridvalues$z)
    args$z <- t(matrix(data=gridvalues$z, 
                       ncol=length(args$x), 
                       nrow=length(args$y), byrow = F))
  } else {
    args$z <- t(matrix(data=x[,variable], 
                       ncol=the.grid$dim[1], 
                       nrow=the.grid$dim[2], byrow = F))
  }
  
  if (is.null(args$theta)) args$theta <- -20 else args$theta <- args$theta
  if (is.null(args$phi)) args$phi <- 20 else args$phi <- args$phi
  if (is.null(args$expand)) args$expand <- 0.4 else args$expand <- args$expand
  if (is.null(args$xlab)) args$xlab <- colnames(the.grid$coord)[1] else args$xlab <- args$xlab
  if (is.null(args$ylab)) args$ylab <- colnames(the.grid$coord)[2] else args$ylab <- args$ylab
  if (is.null(args$zlab) & is.character(variable)) args$zlab <- variable
  if (is.null(args$zlab)) args$zlab <- colnames(x)[variable] else args$zlab <- args$zlab
  if (is.null(args$lwd)) args$lwd <- 1.5 else args$lwd <- args$lwd
  if (is.null(args$col)) args$col <- gg_color(1)
  if (is.null(args$border)) args$border <- "grey"
  if (is.null(args$main)) args$main <- paste0("Values of prototypes for ", args$varname)
  if (is.null(args$minsize)) args$minsize <- 0.5 # for points
  if (is.null(args$maxsize)) args$maxsize <- 2 # for points
  args$varname <- NULL
  if(the.grid$topo=="hexagonal") {
    args$sub <- "Hexagonal topography : linear interpolation between points"
  }
  pmat <- do.call("persp", args)
  
  # Adding points to the map
  # The following taken from : 
  # https://stackoverflow.com/questions/28062399/r-add-points-to-surface-plot-with-persp-having-the-appropriate-size
  xx = the.grid$coord[,1]
  yy = the.grid$coord[,2]
  zz = x[,variable]
  
  # determine distance to eye
  psize = depth3d(xx,yy,zz,pmat,minsize=args$minsize, maxsize = args$maxsize)
  # from 3D to 2D coordinates
  mypoints <- trans3d(xx, yy, zz, pmat=pmat)
  # plot in 2D space with pointsize related to distance
  points(mypoints, pch=16, cex=psize, col="black")
}


coords_polydist <- function(ind, values, the.grid) {
  cur.coords <- the.grid$coord[ind,]
  cur.values <- values[[ind]]
  neigh <- as.numeric(as.character(names(values[[ind]])))
  
  maxnbneigh <- 8
  if(the.grid$topo=="hexagonal"){
    maxnbneigh <- 6
  }
  
  poly.coord <- matrix(nrow=length(neigh), ncol=2)
  for(i in 1:length(neigh)){
      n.coords <- the.grid$coord[neigh[i],]
      poly.coord[i,] <- cur.coords + (n.coords-cur.coords)*cur.values[i]
  }
  # For borders
  if(length(neigh)<maxnbneigh){
    poly.coord <- rbind(poly.coord, cur.coords)
  }
  
  poly.coord <- data.frame(poly.coord)
  colnames(poly.coord) <- c("x", "y")
  
  # Sort poly.coord in clockwise order. 
  # Code from https://github.com/skgrange/gissr
  x_centre <- mean(poly.coord$x)
  y_centre <- mean(poly.coord$y)
  # Calculate deltas
  poly.coord$x_delta <- poly.coord$x - x_centre
  poly.coord$y_delta <- poly.coord$y - y_centre
  
  # Resolve angle, in radians
  poly.coord$angle <- atan2(poly.coord$y_delta, poly.coord$x_delta)
  poly.coord <- poly.coord[order(poly.coord$angle, decreasing = TRUE), ]

  # Drop intermediate variables
  poly.coord[, c("x_delta", "y_delta", "angle")] <- NULL
  poly.coord$id <- ind
  rownames(poly.coord) <- paste0(ind, ".", 1:nrow(poly.coord))

  return(poly.coord)
}



### SOM algorithm graphics
plotPrototypes <- function(sommap, type, variable, my.palette, show.names,
                           names, is.scaled, view, args) {
  ## types : 3d, lines, barplot, color, smooth.dist, poly.dist, umatrix,
  # mds
  
  # default value for type="lines"
  if (!is.element(type,c("3d","lines","barplot","color", "poly.dist",
                         "umatrix", "smooth.dist", "mds", "grid.dist"))) {
    warning("incorrect type replaced by 'lines'\n", call.=TRUE, 
            immediate.=TRUE)
    type <- "lines"
  }
  # relational control
  if (sommap$parameters$type=="relational" && type %in% c("color", "3d"))
    stop("prototypes/", type, " plot is not available for 'relational'\n", 
         call.=TRUE)
  
  if(type %in% c("3d", "color")){
    if (length(variable)>1) {
      warning("length(variable)>1, only first element will be considered\n", 
              call.=TRUE, immediate.=TRUE)
      variable <- variable[1]
    }
  }
  
  tmp.var <- variable
  if (sommap$parameters$type=="korresp" & (is.numeric(variable))) {
    if(view=="r"){
      tmp.var <- rownames(sommap$data)[variable]
    } else tmp.var <- colnames(sommap$data)[variable]
  } 
  
  if (type=="lines" || type=="barplot") {
    ggplotFacet("prototypes", type, sommap$prototypes[,variable], as.numeric(rownames(sommap$prototypes)),
                show.names, names, is.scaled,
                sommap$parameters$the.grid, args)
  } else if (type=="color" | type=="3d") {
    args$varname <- tmp.var
    if(type=="color"){
      ggplotGrid("prototypes", "color", sommap$prototypes[,tmp.var], 
                 as.numeric(rownames(sommap$prototypes)), show.names, 
                 names, sommap$parameters$the.grid, args)
    } else if (type=="3d"){
      plot3d(sommap$prototypes, sommap$parameters$the.grid, type, tmp.var, args)
    }

  } else if (type=="3d") {
    tmp.var <- variable
    if (sommap$parameters$type=="korresp" & (is.numeric(variable))) {
      if (view=="r") tmp.var <- variable+ncol(sommap$data)
    }
    plot3d(sommap$prototypes, sommap$parameters$the.grid, type, tmp.var, args)
  } else if (type=="poly.dist") {
    values <- protoDist(sommap, "neighbors")
    if (sommap$parameters$type=="relational") {
      if (sum(unlist(values)<0)>0) {
        stop("Impossible to plot 'poly.dist'!", call.=TRUE)
      } else values <- lapply(values,sqrt)
    }
    ggplotGrid("prototypes", type, values, sommap$clustering, show.names,
                   names, sommap$parameters$the.grid, args)
  } else if (type=="umatrix" || type=="smooth.dist") {
    values <- protoDist(sommap, "neighbors")

    if (sommap$parameters$type=="relational") {
      if (sum(unlist(values)<0)>0) {
        stop("Impossible to plot 'smooth.dist'!", call.=TRUE)
      } else values <- lapply(values,sqrt)
    }
    values <- unlist(lapply(values,mean))
    if (type=="umatrix") {
      args$labelcolor <- "umatrix\nbetween\nprototypes"
      args$varname <- variable
      ggplotGrid("prototypes", "color", values,
                 as.numeric(as.character(rownames(sommap$prototypes))), 
                 show.names, names, 
                 sommap$parameters$the.grid, args)
    } else {
      if(sommap$parameters$the.grid$topo=="hexagonal"){
        warning("Hexagonal topograpy: imputing missing values to make a full squared grid\n", call.=TRUE, 
                immediate.=TRUE)
      }
      dataplot <- data.frame(cbind("x"=sommap$parameters$the.grid$coord[,1],
                                 "y"=sommap$parameters$the.grid$coord[,2],
                                 "z"=values))
      ggplot(data=dataplot, aes(x=x, y=y, z=z)) + metR::geom_contour_fill(na.fill = TRUE) + 
      labs(title="Distances between prototypes") + scale_fill_distiller(palette="PRGn")
    }
  } else if (type=="mds") {
    if (sommap$parameters$type=="relational") {
      the.distances <- protoDist(sommap, "complete")
      if (sum(the.distances<0)>0) {
        stop("Impossible to plot 'MDS'!", call.=TRUE)
      } else the.distances <- sqrt(the.distances)
    }
    else the.distances <- dist(sommap$prototypes)
    proj.coord <- cmdscale(the.distances, 2)
    if (is.null(args$labels)) args$labels <- as.character(1:nrow(sommap$prototypes))
    if (is.null(args$cex)) args$cex <- 4
    if(is.null(args$sc)){
      dataplot <- data.frame(x = proj.coord[,1], y = proj.coord[,2], "labels"=args$labels)
      ggplot(dataplot, aes(x=x, y=y)) + geom_text(aes(label=labels), alpha=0.7, size=args$cex, fontface="bold") +
        labs(x="x", y="y", title="Prototypes visualization by MDS")
    } else {
      dataplot <- data.frame(x = proj.coord[,1], y = proj.coord[,2], "labels"=args$labels, "Super_Cluster"=as.factor(args$sc))
      ggplot(dataplot, aes(x=x, y=y)) + geom_text(aes(label=labels, col=Super_Cluster), alpha=0.7, size=args$cex, fontface="bold") +
        labs(x="x", y="y", title="Prototypes visualization by MDS")
    }
   
  } else if (type=="grid.dist") {
    if (sommap$parameters$type=="relational") {
      the.distances <- protoDist(sommap, "complete")
     if (sum(the.distances<0)>0) {
      stop("Impossible to plot 'grid.dist'!", call.=TRUE)
     } else {
       the.distances <- sqrt(the.distances)
       the.distances <- the.distances[lower.tri(the.distances)]
     }
    } else the.distances <- dist(sommap$prototypes)
    dataplot <- data.frame("x" = as.vector(the.distances), 
                           "y" = as.vector(dist(sommap$parameters$the.grid$coord)))
    ggplot(dataplot, aes(x=x, y=y)) + 
      geom_point(shape=21, alpha=0.7, size=1) +
      labs(x="prototype distances", y="grid distances", 
           title="Distances between protoypes (input space vs. grid)")
  } else stop("Sorry: this type is still to be implemented.", call.=TRUE)
}

plotObs <- function(sommap, type, variable, my.palette, show.names, names,
                    is.scaled, view, args) {
  ## types : hitmap, lines, names, color, barplot, boxplot
  # default value is type="hitmap"
  if (!is.element(type,c("hitmap", "lines", "names", "color",
                         "barplot", "boxplot"))) {
    warning("incorrect type replaced by 'hitmap'\n", call.=TRUE, 
            immediate.=TRUE)
    type <- "hitmap"
  }
  # korresp control
  if (sommap$parameters$type=="korresp" && !(type%in%c("hitmap", "names"))) {
    warning("korresp SOM: incorrect type replaced by 'hitmap'\n", call.=TRUE, 
            immediate.=TRUE)
    type <- "hitmap"
  }
  # relational control
  if (sommap$parameters$type=="relational" && !(type%in%c("hitmap", "names"))) {
    warning("relational SOM: incorrect type replaced by 'hitmap'\n", 
            call.=TRUE, immediate.=TRUE)
    type <- "hitmap"
  }
  
  if(type %in% c("lines", "barplot", "boxplot", "names")){
    if (type == "lines" || type == "barplot" || type == "boxplot") {
      values <- sommap$data[,variable]
    } else if (type=="names") {
      if (sommap$parameters$type %in% c("relational", "korresp")) {
        values <- names(sommap$clustering)
        args$varname <- "names"
      }
      if(sommap$parameters$type=="numeric") {
        if (length(variable)>1) {
          warning("length(variable)>1, only first element will be considered\n", 
                  call.=TRUE, immediate.=TRUE)
          variable <- variable[1]
        }
        if(is.null(variable)) variable <- "row.names"
        if(variable=="row.names"){
            values <- names(sommap$clustering)
        } else {
          if(is.numeric(variable)) variable <- colnames(sommap$data)[variable]
            values <- sommap$data[,variable]
        }
        args$varname <- variable
      }
    }
    ggplotFacet("obs", type, values, sommap$clustering,
                show.names, names, is.scaled,
                sommap$parameters$the.grid, args)
  } else if(type %in% c("color", "hitmap")){
      if (type=="color") {
      if (length(variable)>1) {
        warning("length(variable)>1, only first element will be considered\n", 
                call.=TRUE, immediate.=TRUE)
        variable <- variable[1]
      } 
        if (sommap$parameters$type=="korresp" & (is.numeric(variable))) {
          if(view=="r"){
            tmp.var <- rownames(sommap$data)[variable]
          } else tmp.var <- colnames(sommap$data)[variable]
        } else if (sommap$parameters$type=="numeric" & (is.numeric(variable))) {
          tmp.var <- colnames(sommap$data)[variable]
        } else tmp.var <- variable
        args$varname <- tmp.var
      values <- sommap$data[,tmp.var]
    }  else if (type=="hitmap") {
      values <- sommap$clustering
    }
    ggplotGrid("obs", type, values, sommap$clustering,
               show.names, names, sommap$parameters$the.grid, args)
  }
}



projectFactor <- function(the.graph, clustering, the.factor, pie.color=NULL) {

  if (!is.factor(the.factor)) the.factor <- as.factor(the.factor)
  vertex.pie <- lapply(split(the.factor, factor(clustering)), table)
  if (is.null(pie.color)) {
    pie.color <- gg_color(nlevels(the.factor))
    # pie.color <- list(c(brewer.pal(8,"Set2"),
    #                     brewer.pal(12,"Set3"))[1:nlevels(the.factor)])
  }
  return(list("vertex.pie"=vertex.pie, "vertex.pie.color"=pie.color))
}

plotProjGraph <- function(proj.graph, show.names=FALSE, names=NULL, 
                          s.radius=1, pie.graph=FALSE, pie.variable=NULL, ...) {
  
  args <- list(...)
  args$x <- proj.graph
  args$edge.width <- E(proj.graph)$weight/max(E(proj.graph)$weight)*10
  if (is.null(s.radius)) s.radius <- 1
  args$vertex.size <- s.radius*20*sqrt(V(proj.graph)$size)/
    max(sqrt(V(proj.graph)$size))
  if (is.null(args$vertex.label) & !show.names) args$vertex.label <- NA
  if (show.names) {
    if (is.null(args$vertex.label)) {
      if (is.null(names)) {
        args$vertex.label <- V(proj.graph)$name
      } else 
        args$vertex.label <- names[as.numeric(V(proj.graph)$name)]
    }
  }
    
  if (args$vertex.shape!="pie") {
    if (is.null(args$vertex.color))
      args$vertex.color <- gg_color(1)
    if (is.null(args$vertex.frame.color))
      args$vertex.frame.color <- gg_color(1)
  }

  par(bg="white")
  do.call("plot.igraph", args)
}

plotAdd <- function(sommap, type, variable, proportional, my.palette,
                    show.names, names, is.scaled, s.radius, pie.graph,
                    pie.variable, args) {
  ## types : pie, color, lines, boxplot, names, words, graph, barplot
  # to be implemented: graph

  # default value is type="pie"
  if (!is.element(type,c("pie", "color", "lines", "barplot", "words",
                         "boxplot", "names", "graph"))) {
    warning("incorrect type replaced by 'pie'\n", call.=TRUE, 
            immediate.=TRUE)
    type <- "pie"
  }
  if (is.null(variable)) {
    stop("for what='add', the argument 'variable' must be supplied\n", 
         call.=TRUE)
  }
  
  # korresp control
  if (sommap$parameters$type=="korresp") 
    stop("graphics of type 'add' do not exist for 'korresp'\n", call.=TRUE)
  
  if(type!="graph" && nrow(variable)!=nrow(sommap$data)){
    stop("length of additional variable does not fit length of the original
         data", call.=TRUE)
  }

  # Numerical variable control
  if(type %in% c("lines", "barplot", "boxplot", "words", "color")){
    nonum <- sapply(variable, is.numeric)
    nonum <- length(nonum[nonum==F])
    if(nonum>0){
      stop(paste(type, "plots are for numerical variables only"), call.=TRUE)
    }
  }
  
  # switch between different types
  if(type %in% c("pie", "lines", "barplot", "boxplot", "names", "words")){
    if(type == "pie") variable <- as.factor(variable)
    if (type=="words") {
      if (is.null(colnames(variable))) {
        stop("no colnames for 'variable'", call.=TRUE)
      }
    }
    ggplotFacet("add", type, values=variable, sommap$clustering, show.names, names, 
                is.scaled, sommap$parameters$the.grid, args)
  } else if(type == "color"){
    ggplotGrid("add", type, values=variable, sommap$clustering, show.names, names, 
               sommap$parameters$the.grid, args)
  } else if (type=="graph") {
    # controls
    if (!is_igraph(variable)){
      stop("for type='graph', argument 'variable' must be an igraph object\n", 
           call.=TRUE)
    }
    if (length(V(variable)) != nrow(sommap$data)){
      stop("length of additional variable does not fit length of the original
         data", call.=TRUE)
    }
    # case of pie
    if (pie.graph) {
      if (is.null(pie.variable)) 
        stop("pie.graph is TRUE, you must supply argument 'pie.variable'\n", 
             call.=TRUE)
      
      if (nrow(as.matrix(pie.variable)) != nrow(sommap$data)) {
        stop("length of argument 'pie.variable' does not fit length of the 
             original data", call.=TRUE)
      }
      
      args$vertex.shape <- "pie"
      if (is.null(args$vertex.pie.color)) args$vertex.pie.color <- NULL
      proj.pie <- projectFactor(variable, sommap$clustering, pie.variable,
                                pie.color=args$vertex.pie.color)
      args$vertex.pie <- proj.pie$vertex.pie
      args$vertex.pie.color <- proj.pie$vertex.pie.color
    } else if (is.null(args$vertex.shape)) args$vertex.shape <- "circle"
    
    # create projected graph and plot
    args$proj.graph <- projectGraph(variable, sommap$clustering, 
                                    sommap$parameters$the.grid$coord)
    args$show.names <- show.names
    args$names <- names
    args$s.radius <- s.radius
    args$pie.graph <- pie.graph
    args$pie.variable <- pie.variable
    do.call("plotProjGraph", args)
  } else 
    stop("Sorry: this type is still to be implemented.", call.=TRUE)
}

#' @title Plot a \code{somRes} class object
#' @name plot.somRes
#' @export
#' 
#' @description Produce graphics to help interpreting a \code{somRes} object.
#' 
#' @param x A \code{somRes} class object.
#' @param what What you want to plot. Either the observations (\code{obs}, 
#' default case), the evolution of energy (\code{energy}), the prototypes 
#' (\code{prototypes}) or an additional variable (\code{add}).
#' @param type Further argument indicating which type of chart you want to have.
#' Choices depend on the value of \code{what} (\code{what="energy"} has no
#' \code{type} argument). Default values are \code{"hitmap"} for \code{obs}, 
#' \code{"color"} for \code{prototypes} and \code{"pie"} for \code{add}. See
#' section ``Details'' below for further details.
#' @param variable Either the variable to be used for \code{what="add"} or the 
#' index of the variable of the data set to consider. For \code{type="boxplot"}, 
#' the default value is the sequence from 1 to the minimum between 5 and the 
#' number of columns of the data set. In all other cases, default value is 1. 
#' See \code{\link{somRes.plotting}} for further details.
#' @param my.palette A vector of colors. If omitted, predefined palettes are 
#' used, depending on the plot case. This argument is used for the following
#' combinations: all \code{"color"} types and \code{"prototypes"/"poly.dist"}.
#' @param is.scaled A boolean indicating whether values should be scaled prior 
#' to plotting or not. Default value is \code{TRUE} when \code{type="numeric"} 
#' and \code{FALSE} in the other cases.
#' @param show.names Boolean used to indicate whether each neuron should have a
#' title or not. Default value is \code{FALSE}. It is feasible on the following 
#' cases: all \code{"color"} types, all \code{"lines"} types, all 
#' \code{"barplot"} types, all \code{"boxplot"} types,
#' all \code{"names"} types, \code{"add"/"pie"}, \code{"prototypes"/"umatrix"}, 
#' \code{"prototypes"/"poly.dist"} and \code{"add"/"words"}.
#' @param the.titles The titles to be printed for each neuron if 
#' \code{show.names=TRUE}. Default to a number which identifies the neuron.
#' @param proportional Boolean used when \code{what="add"} and 
#' \code{type="pie"}. It indicates if the pies should be proportional to the 
#' number of observations in the class. Default value is \code{TRUE}.
#' @param s.radius The size of the pies to be plotted (maximum size when 
#' \code{proportional=TRUE}). The default value is \code{0.9}.
#' @param pie.graph Boolean used when \code{what="add"} and \code{type="graph"}. 
#' It indicates if the vertices should be pies or not.
#' @param pie.variable The variable needed to plot the pies when 
#' \code{what="add"}, \code{type="graph"} and argument \code{pie.graph=TRUE}.
#' @param view Used only when the algorithm's type is \code{"korresp"}. It
#' indicates whether rows (\code{"r"}) or columns (\code{"c"}) must be drawn.
#' @param \dots Further arguments to be passed to the underlined plot function
#' (which can be \code{\link{plot}}, \code{\link{barplot}}, \code{\link{pie}}...
#' depending on \code{type}; see \code{\link{somRes.plotting}} for further
#' details).
#' 
#' @details See \code{\link{somRes.plotting}} for further details.
#' 
#' @author Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{trainSOM}} to run the SOM algorithm, that returns a 
#' \code{somRes} class object.

plot.somRes <- function(x, what=c("obs", "prototypes", "energy", "add"), 
                        type=switch(what,
                                    "obs"="hitmap",
                                    "prototypes"="color",
                                    "add"="pie",
                                    "energy"="energy"),
                        # variable = if (what=="add" | type=="names") NULL else 1:ncol(x$data),
                        variable = NULL,
                        my.palette=NULL, 
                        is.scaled = if (x$parameters$type=="numeric") TRUE else
                          FALSE,
                        show.names=TRUE, 
                        names=if (what!="energy")
                          switch(type,
                                 "graph"=1:prod(x$parameters$the.grid$dim),
                                 1:prod(x$parameters$the.grid$dim)),
                        proportional=TRUE, s.radius=1, pie.graph=FALSE, 
                        pie.variable=NULL,
                        view = if (x$parameters$type=="korresp") "r" else NULL,
                        ...) {
  args <- list(...)
  what <- match.arg(what)

  calls <- names(sapply(match.call(), deparse))[-1]
  if(any("print.title" %in% calls)) {
    warning("'print.title' will be deprecated, please use 'show.names' instead", call. = F, immediate. = T)
    show.names <- args$print.title
  }
  if(any("the.titles" %in% calls)) {
    warning("'the.titles' will be deprecated, please use 'names' instead", call. = F, immediate. = T)
    names <- args$the.titles
  }
  
  if(is.null(variable)==F){
    if(is.null(args$varname)) args$varname <- deparse(substitute(variable))
  }
  
  if(is.null(variable)){
    if (what!="add" & type!="names"){
      if(x$parameters$type %in% c("numeric", "relational")){
        variable <- colnames(x$data)[1:ncol(x$data)]
      } else if(x$parameters$type == "korresp"){
        if(view=="c") variable <- 1:ncol(x$data) else variable <- 1:nrow(x$data)
      } 
    }
  }

  if ((x$parameters$type=="korresp")&&!(view%in%c("r","c")))
      stop("view must be one of 'r'/'c'",call.=TRUE)
  if (length(names)!=prod(x$parameters$the.grid$dim) & what!="energy") {
    names=switch(type,
                      "graph"=1:prod(x$parameters$the.grid$dim),
                      1:prod(x$parameters$the.grid$dim))
    warning("unadequate length for 'names'; replaced by default",
            call.=TRUE, immediate.=TRUE)
  }
  switch(what,
         "prototypes"=plotPrototypes(x, type, variable, my.palette, show.names,
                                     names, is.scaled, view, args),
         "energy"=ggplotEnergy(x),
         "add"=plotAdd(x, type, if (type!="graph") as.matrix(variable) else 
           variable, proportional, my.palette, show.names, names, 
                       is.scaled, s.radius, pie.graph, pie.variable, args),
         "obs"=plotObs(x, type, variable, my.palette, show.names, names,
                       is.scaled, view, args))
}