# Functions to handle graphs in SOM ###########################################

#' @title Compute the projection of a graph on a grid
#' @name projectIGraph
#' @aliases projectIGraph.somRes
#' @export
#' 
#' @description Compute the projection of a graph, provided as an 
#' \code{\link[igraph]{igraph}} object, on the grid of the \code{somRes} object.
#' 
#' @param object a \code{somRes} object.
#' @param init.graph an \link[igraph]{igraph} whose number of vertices is equal
#' to the clustering length of the \code{somRes} object.
#' @param \dots Not used.
#' 
#' @return The result is an \code{\link[igraph]{igraph}} which vertexes are the
#' clusters (the clustering is thus understood as a vertex clustering) and the 
#' edges are the counts of edges in the original graph between two vertices
#' corresponding to the two clusters in the projected graph or, if 
#' \code{init.graph} is a weighted graph, the sum of the weights between the 
#' pairs of vertices corresponding to the two clusters.
#' 
#' The resulting igraph object's attributes are: \itemize{
#'   \item the graph attribute \code{layout} which provides the layout of the 
#'   projected graph according to the grid of the SOM;
#'   \item the vertex attributes \code{name} and \code{size} which, respectively
#'   are the vertex number on the grid and the number of vertexes included in 
#'   the corresponding cluster;
#'   \item the edge attribute \code{weight} which gives the number of edges (or 
#'   the sum of the weights) between the vertexes of the two corresponding 
#'   clusters.
#' }
#' 
#' @author Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @references 
#' Olteanu M., Villa-Vialaneix N. (2015) Using SOMbrero for clustering and 
#' visualizing graphs. \emph{Journal de la Société Française de Statistique},
#' \strong{156}, 95-119.
#' 
#' @seealso \code{\link{projectIGraph.somSC}} which uses the results of a 
#' super-clustering to obtain another projected graph. \code{\link{plot.somRes}} 
#' with the option \code{type="graph"} or \code{\link{plot.somSC}} with the 
#' option \code{type="projgraph"}.
#' 
#' @examples 
#' data(lesmis)
#' set.seed(7383)
#' mis.som <- trainSOM(x.data=dissim.lesmis, type="relational", nb.save=10)
#' proj.lesmis <- projectIGraph(mis.som, lesmis)
#' \dontrun{plot(proj.lesmis)}

projectIGraph <- function(object, init.graph, ...) {
  UseMethod("projectIGraph")
}

#' @export

projectIGraph.somRes <- function(object, init.graph, ...) {
  if (!is_igraph(init.graph)) {
    stop("'init.graph' must be an igraph object\n", call.=TRUE)
  }
  if (gorder(init.graph)!=length(object$clustering)) {
    stop("The number of vertexes of 'init.graph' does not match the clustering length\n", call.=TRUE)
  }
  proj.graph <- projectGraph(init.graph, object$clustering,
                             object$parameters$the.grid$coord)
  return(proj.graph)
}

projectGraph <- function(the.graph, clustering, coord.clustering) {
  ## TODO: handle directed graph...
  ## If directed, convert into undirected
  if (is.directed(the.graph)) the.graph <- as.undirected(the.graph)
  
  all.neurons <- 1:nrow(coord.clustering)
  nonempty.neurons <- sort(unique(clustering))
  p.edges <- NULL # list of edges
  p.edges.weights <- NULL # weights of the edges
  if (is.null(V(the.graph)$size)) { # number of nodes
    v.sizes <- as.vector(table(clustering))
  } else {
    v.sizes <- tapply(V(the.graph)$size, clustering, sum)
  }
  
  for (neuron in nonempty.neurons) {
    v.neuron <- as.vector(V(the.graph)[which(clustering == neuron)])
    for (neuron2 in setdiff(nonempty.neurons, 1:neuron)) {
      v.neuron2 <- as.vector(V(the.graph)[which(clustering == neuron2)])
      if (is.null(E(the.graph)$weight)) {
        nb.edges <- length(E(the.graph)[v.neuron %--% v.neuron2])
      } else {
        nb.edges <- sum(E(the.graph)[v.neuron %--% v.neuron2]$weight)
      }
      if (nb.edges > 0) {
        p.edges <- c(p.edges, neuron, neuron2)
        p.edges.weights <- c(p.edges.weights, nb.edges)
      }
    }
  }
  proj.graph <- 
    graph_from_data_frame(matrix(p.edges, ncol = 2, byrow = TRUE),
                          directed = FALSE, 
                          vertices = data.frame("name" = nonempty.neurons,
                                                "size" = v.sizes))
  E(proj.graph)$weight <- p.edges.weights
  proj.graph <- set_graph_attr(proj.graph, "layout",
                               coord.clustering[nonempty.neurons, ])
  return(proj.graph)
}