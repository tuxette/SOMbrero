## These functions handle maps (creation, description, display...)
## ----------------------------------------------------------------------------
#' @title Create an empty grid
#' @export
#'
#' @description
#' Create an empty (square) grid equipped with topology.
#'
#' @param dimension a 2-dimensional vector giving the dimensions (width, length)
#' of the grid
#' @param topo topology of the grid. Accept values \code{"square"} (Default) or \code{"hexagonal"}.
#' @param dist.type distance type that defines the topology of the grid (see
#' 'Details'). Default to \code{"euclidean"}
#'
#' @author Madalina Olteanu <madalina.olteanu@univ-paris1.fr>\cr
#' Nathalie Vialaneix <nathalie.vialaneix@inrae.fr>
#'
#' @details The units (neurons) of the grid are positionned at coordinates 
#' (1,1), (1,2), (1,3), ..., (2,1), (2,2), ..., for the \code{square} topology.
#' The topology of the map is defined by a distance based on those coordinates, 
#' that can be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, 
#' \code{"canberra"}, \code{"minkowski"}, \code{"letremy"}, where the first 5 
#' ones correspond to distance methods implemented in \code{\link{dist}} and 
#' \code{"letremy"} is the distance of the original implementation by Patrick 
#' Letremy that switches between \code{"maximum"} and \code{"euclidean"} during
#' the training.
#'
#' @seealso \code{\link{plot.myGrid}} for plotting the grid
#'
#' @references Letrémy P. (2005) Programmes basés sur l'algorithme de Kohonen 
#' et dédiés à l'analyse des données. SAS/IML programs for 'korresp'. 
#' \url{http://samm.univ-paris1.fr/Programmes-SAS-de-cartes-auto}
#'
#' @examples
#' initGrid()
#' initGrid(dimension=c(5, 7), dist.type = "maximum")
#'
#' @return an object of class \code{myGrid} with the following entries:
#' \itemize{
#'   \item{\code{coord}} 2-column matrix with x and y coordinates of the grid 
#'   units
#'   \item{\code{topo}} topology of the grid;
#'   \item{\code{dim}} dimensions of the grid (width corresponds to x 
#'   coordinates)
#'   \item{\code{dist.type}} distance type that defines the topology of the 
#'   grid.
#' }

initGrid <- function(dimension = c(5,5), topo = c("square", "hexagonal"),
                     dist.type = c("euclidean", "maximum", "manhattan",
                                   "canberra", "minkowski", "letremy")) {
  # check options
  topo <- match.arg(topo)
  dist.type <- match.arg(dist.type)

  # create coordinates for neurons
    x <- 1:dimension[1]
    y <- 1:dimension[2]
    tmp <- as.matrix(expand.grid(x, y))
    tmp <- tmp[order(tmp[,1]),]
  if(topo == "hexagonal"){
    tmp[, 1] <- tmp[, 1] + 0.5 * (tmp[, 2]%%2)
    tmp[, 2] <- sqrt(3)/2 * tmp[, 2]
  }
  colnames(tmp) <- c("x", "y")
  result <- list("coord" = tmp, "topo" = topo, "dim" = dimension,
                 "dist.type" = dist.type)
  class(result) <- "myGrid"

  return(result)
}

## Methods for objects of class 'myGrid'
#' @title Methods for 'myGrid' objects.
#' @name myGrid
#' @exportClass myGrid
#' @export
#'
#' @aliases summary.myGrid
#' @aliases print.myGrid
#' @aliases plot.myGrid
#' @aliases myGrid-class
#'
#' @description Methods for the result of \code{\link{initGrid}}
#' (\code{myGrid} object)
#' @param object \code{myGrid} object
#' @param x \code{myGrid} object
#' @param neuron.col Color(s) used to depict the neurons. Default value is 
#' \code{white}. If the argument is composed of one single color, neurons will 
#' all be filled with the same color. If the argument is composed of many 
#' colors, the number of colors must match the total number of neurons.
#' @param show.names Whether the cluster names must be printed in center of
#' the grid or not. Default to \code{TRUE} (names not displayed).
#' @param names If \code{show.names = TRUE}, values of the names to 
#' display. Default to the cluster number.
#' @param \dots Further arguments to the \code{\link{plot}} function.
#' 
#' @author Madalina Olteanu, \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix, \email{nathalie.vialaneix@inrae.fr}
#' 
#' @details The \code{myGrid} class has the following entries:
#' \itemize{
#'   \item{\code{coord}} 2-column matrix with x and y coordinates of the grid 
#'   units
#'   \item{\code{topo}} topology of the grid;
#'   \item{\code{dim}} dimensions of the grid (width corresponds to x 
#'   coordinates)
#'   \item{\code{dist.type}} distance type that defines the topology of the 
#'   grid.
#' }
#' During plotting, the color filling process uses the coordinates of the object
#' \code{x} included in \code{x$coord}.
#'
#' @seealso \code{\link{initGrid}} to define a \code{myGrid} class object.
#' 
#' @examples
#' # creating grid
#' a.grid <- initGrid(dimension=c(5,5), topo="square", dist.type="maximum")
#' 
#' # plotting grid
#' # without any color specification
#' plot(a.grid)
#' # generating colors from rainbow() function
#' my.colors <- rainbow(5*5)
#' plot(a.grid) + scale_fill_manual(my.colors)

print.myGrid <-function(x,...) {
  cat("\n      Self-Organizing Map structure\n\n")
  cat("        Features   :\n")
  cat("           topology     : ", x$topo, "\n")
  cat("           x dimension  : ", x$dim[1], "\n")
  cat("           y dimension  : ", x$dim[2], "\n")
  cat("           distance type: ", x$dist.type, "\n")
  cat("\n")
}

#' @export
#' @rdname myGrid
summary.myGrid <- function(object,...) {
  cat("\nSummary\n\n")
  cat("      Class            : ", class(object), "\n")
  print(object)
}

#' @export
#' @rdname myGrid
plot.myGrid <- function(x, show.names = TRUE, 
                        names = 1:prod(x$dim), ...) {
  # get graphical parameters
  args <- list(...)
  
  calls <- names(sapply(match.call(), deparse))[-1]
  if(any("print.title" %in% calls)) {
    warning("'print.title' will be deprecated, please use 'show.names' instead", call. = F, immediate. = T)
    show.names <- args$print.title
  }
  if(any("the.titles" %in% calls)) {
    warning("'the.titles' will be deprecated, please use 'names' instead", call. = F, immediate. = T)
    names <- args$the.titles
  }
  
  dimgrid <- prod(x$dim)
  values <- 1:dimgrid
  id <- 1:dimgrid
  if(is.null(args$main)) args$main <- "Grid"

  tp <- ggplotGrid("obs", "grid", values, id,
                   show.names, names, x, args) + 
        theme(legend.position = "none")
  tp
}