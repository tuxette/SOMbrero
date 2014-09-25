\name{projectIGraph}
\alias{projectIGraph.somRes}
\alias{projectIGraph}

\title{
Compute the projection of a graph on a grid
}

\description{Compute the projection of a graph, provided as an 
\code{\link[igraph]{igraph}} object, on the grid of the \code{somRes} object.}

\usage{
\method{projectIGraph}{somRes}(object, init.graph, \ldots)
}


\arguments{
  \item{object}{a \code{somRes} object.}
  \item{init.graph}{an \link[igraph]{igraph} whose number of vertices is equal
  to the clustering length of the \code{somRes} object.}
  \item{\ldots}{Not used.}
}

\value{
The result is an \code{\link[igraph]{igraph}} which vertexes are the clusters
(the clustering is thus understood as a vertex clustering) and the edges are the
counts of edges in the original graph between two vertices corresponding to the
two clusters in the projected graph.

The resulting igraph object's attribute are: \itemize{
\item the graph attribute \code{layout} which provides the layout of the 
projected graph according to the grid of the SOM;
\item the vertex attributes \code{name} and \code{sizes} which, respectively 
are the vertex number on the grid and the number of vertexes included in the
corresponding cluster;
\item the edge attribute \code{nb.edges} which gives the number of edges 
between the vertexes of the two corresponding clusters.}
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@toulouse.inra.fr}
}

\seealso{\code{\link{plot.somRes}} with the option \code{type="graph"}.}

\examples{
data(lesmis)
set.seed(7383)
mis.som <- trainSOM(x.data=dissim.lesmis, type="relational", nb.save=10)
proj.lesmis <- projectIGraph(mis.som, lesmis)
\dontrun{plot(proj.lesmis)}
}

\keyword{methods}
