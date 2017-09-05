## These functions handle grids (creation, description, display...)

initGrid <- function(dimension=c(5,5), topo=c("square"),
                     dist.type=c("euclidean","maximum","manhattan",
                                 "canberra","binary","minkowski","letremy")) {
  topo <- match.arg(topo)
  dist.type <- match.arg(dist.type)
  if(topo=="square") {
    x <- seq(from=1, to=dimension[1], by=1)
    y <- seq(from=1, to=dimension[2], by=1)
    tmp <- as.matrix(expand.grid(y,x))
    tmp <- tmp[order(tmp[,1]),]
    colnames(tmp) <- c("x","y")
  }
  ## TODO Implement other grids such as hexagonal
  result <- list("coord"=tmp, "topo"=topo, "dim"=dimension,
                 "dist.type"=dist.type)
  class(result) <- "myGrid"
  return(result)
}

print.myGrid <-function(x,...) {
  cat("\n      Self-Organizing Map structure\n\n")
  cat("        Features   :\n")
  cat("           topology     : ", x$topo, "\n")
  cat("           x dimension  : ", x$dim[1], "\n")
  cat("           y dimension  : ", x$dim[2], "\n")
  cat("           distance type: ", x$dist.type, "\n")
  cat("\n")
}

summary.myGrid <- function(object,...) {
  cat("\nSummary\n\n")
  cat("      Class            : ", class(object),"\n")
  print(object)
}

plot.myGrid <- function(x, neuron.col = "white", print.title = FALSE, 
                        the.titles = paste("Cluster", 1:prod(x$dim)), ...) {
  # get graphical parameters
  args <- list(...)
  
  # default parameter: new graphic
  omar <- par()$mar
  on.exit(par(mar=omar))
  plane.mar <- c(0.5,0.5,1,0.5)
  par(mar=plane.mar)
  
  # default parameter: squares topology
  ## TODO implement other topologies such as hexagonal
  if (x$topo=="square") {
    basesize <- 0.5
    xleft <- (x$coord[,1]-basesize)
    xright <- (x$coord[,1]+basesize)
    ybottom <- (x$coord[,2]-basesize)
    ytop <- (x$coord[,2]+basesize)
    
    plot.args <- c(list(x=NA, xlim=range(xleft,xright), 
                        ylim=range(ybottom,ytop), xlab="", ylab="", axes=FALSE,
                        type="n"),
                   args)
    do.call("plot",plot.args)
    
    # default parameter: neuron.col=white
    if(length(neuron.col)!=prod(x$dim) & length(neuron.col) > 1) {
      warning("unadequate number of colors; default color will be used\n", 
              immediate.=TRUE, call.=TRUE)
      neuron.col <- "white"
    }
    rect(xleft,ybottom,xright,ytop,col=neuron.col)
    
    if (print.title) {
      text(x = x$coord[,1], y = x$coord[,2], labels = the.titles, cex = 0.7)
    }
  }
}