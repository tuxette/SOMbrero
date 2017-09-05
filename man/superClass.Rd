\name{superClass}
\alias{superClass}
\alias{superClass.somRes}
\alias{print.somSC}
\alias{summary.somSC}
\alias{projectIGraph.somSC}
\alias{plot.somSC}

\title{Create super-clusters from SOM clustering}

\description{
Aggregate the resulting clustering of the SOM algorithm into super-clusters.}

\usage{
\method{superClass}{somRes}(sommap, method="ward.D", members=NULL, k=NULL, 
h=NULL, \ldots)
\method{print}{somSC}(x, \ldots)
\method{summary}{somSC}(object, \ldots)
\method{projectIGraph}{somSC}(object, init.graph, \ldots)
\method{plot}{somSC}(x, type=c("dendrogram", "grid", "hitmap", "lines", 
               "barplot", "boxplot", "mds", "color", "poly.dist", "pie",
               "graph", "dendro3d", "radar", "projgraph"),
     plot.var=TRUE, plot.legend=FALSE, add.type=FALSE, \ldots)
}

\arguments{
  \item{sommap}{A \code{somRes} object}
  \item{method, members}{Arguments passed to the \code{\link{hclust}} function.}
  \item{k, h}{Arguments passed to the \code{\link{cutree}} function
  (respectively, the number of super-clusters or the height where to cut the
  dendrogram).}
  \item{x, object}{A \code{somSC} object}
  \item{init.graph}{An \link[igraph]{igraph} object which is projected according
  to the super-clusters. The number of vertices of \code{init.graph} must be 
  equal to the number of rows in the original dataset processed by the SOM 
  (case \code{"korresp"} is not handled by this function). In the projected 
  graph, the vertices are positionned at the center of gravity of the 
  super-clusters (more details in the section \strong{Details} below).}
  \item{type}{The type of plot to draw. Default value is \code{"dendrogram"}, 
  to plot the dendrogram of the clustering. Case \code{"grid"} plots the grid 
  in color according to the super clustering. Case \code{"projgraph"} uses an
  \link[igraph]{igraph} object passed to the argument \code{variable} and plots
  the projected graph as defined by the function \code{projectIGraph.somSC}.
  All other cases are those available in the function \code{\link{plot.somRes}} 
  and surimpose the super-clusters over these plots.}
  \item{plot.var}{A boolean indicating whether a graph showing the evolution of 
  the explained variance should be plotted. This argument is only used when 
  \code{type="dendrogram"}, its default value is \code{TRUE}.}
  \item{plot.legend}{A boolean indicating whether a legend should be added to 
  the plot. This argument is only used when \code{type} is either \code{"grid"} 
  or \code{"hitmap"} or \code{"mds"}. Its default value is \code{FALSE}.}
  \item{add.type}{A boolean, which default value is \code{FALSE}, 
  indicating whether you are giving an additional variable to the argument 
  \code{variable} or not. If you do, the function \code{\link{plot.somRes}} 
  will be called with the argument \code{what} set to \code{"add"}.}
  \item{print.title}{Whether the cluster titles must be printed in center of
  the grid or not for \code{type="grid"}. Default to \code{FALSE} (titles not 
  displayed).}
  \item{the.titles}{If \code{print.title = TRUE}, values of the title to 
  display for \code{type="grid"}. Default to "Cluster " followed by the cluster
  number.}
  \item{\ldots}{Used for \code{plot.somSC}: further arguments passed either to
  the function \code{\link{plot}} (case \code{type="dendro"}) or to 
  \code{\link{plot.myGrid}} (case \code{type="grid"}) or to 
  \code{\link{plot.somRes}} (all other cases).}
}

\details{
The \code{superClass} function can be used in 2 ways:
\itemize{
\item to choose the number of super clusters via an \code{\link{hclust}} object: 
then, both arguments \code{k} and \code{h} are not filled.
\item to cut the clustering into super clusters: then, either argument \code{k} 
or argument \code{h} must be filled. See \code{\link{cutree}} for details on 
these arguments.
}
The squared distance between prototypes is passed to the algorithm.

\code{summary} on a \code{superClass} object produces a complete summary of the
results that displays the number of clusters and super-clusters, the clustering
itself and performs ANOVA analyses. For \code{type="numeric"} the ANOVA is
performed for each input variable and test the difference of this variable
accross the super-clusters of the map. For \code{type="relational"} a 
dissimilarity ANOVA is performed (see (Anderson, 2001), except that in the 
present version, a crude estimate of the p-value is used which is based on the
Fisher distribution and not on a permutation test.

On plots, the different super classes are identified in the following ways:
\itemize{
\item either with different color, when \code{type} is set among: 
\code{"grid"} (*, #), \code{"hitmap"} (*, #), \code{"lines"} (*, #), 
\code{"barplot"} (*, #), \code{"boxplot"}, \code{"mds"} (*, #), 
\code{"dendro3d"} (*, #), \code{"graph"} (*, #)
\item or with title, when \code{type} is set among: \code{"color"} (*), 
\code{"poly.dist"} (*, #), \code{"pie"} (#), \code{"radar"} (#)
}
In the list above, the charts available for a \code{korresp} SOM are marked with 
a * whereas those available for a \code{relational} SOM are marked with a #.

\code{projectIGraph.somSC} produces a projected graph from the
\link[igraph]{igraph} object passed to the argument \code{variable} as
described in (Olteanu and Villa-Vialaneix, 2015). The attributes of this graph
are the same than the ones obtained from the SOM map itself in the function
\code{\link{projectIGraph.somRes}}. \code{plot.somSC} used with 
\code{type="projgraph"} calculates this graph and represents it by positionning
the super-vertexes at the center of gravity of the super-clusters. This feature
can be combined with \code{pie.graph=TRUE} to super-impose the information 
from an external factor related to the individuals in the original dataset (or, 
equivalently, to the vertexes of the graph).
}

\references{
Anderson M.J. (2001). A new method for non-parametric multivariate analysis of
variance. \emph{Austral Ecology}, \strong{26}, 32-46.

Olteanu M., Villa-Vialaneix N. (2015) Using SOMbrero for clustering and 
visualizing graphs. \emph{Journal de la Societe Francaise de Statistique}, 
\strong{156}, 95-119.
}

\value{The \code{superClass} function returns an object of class 
\code{somSC} which is a list of the following elements:
\item{cluster}{The super clustering of the prototypes (only if either \code{k} 
or \code{h} are given by user).}
\item{tree}{An \code{\link{hclust}} object.}
\item{som}{The \code{somRes} object given as argument (see 
\code{\link{trainSOM}} for details).}

The \code{projectIGraph.somSC} function returns an object of class 
\code{\link{igraph}} with the following attributes: \itemize{
\item the graph attribute \code{layout} which provides the layout of the 
projected graph according to the center of gravity of the super-clusters
positionned on the SOM grid;
\item the vertex attributes \code{name} and \code{size} which, respectively 
are the vertex number on the grid and the number of vertexes included in the
corresponding cluster;
\item the edge attribute \code{weight} which gives the number of edges (or the
sum of the weights) between the vertexes of the two corresponding clusters.}
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Julien Boelaert \email{julien.boelaert@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}

\seealso{\code{\link{hclust}}, \code{\link{cutree}}, \code{\link{trainSOM}}, 
\code{\link{plot.somRes}}}

\examples{
set.seed(11051729)
my.som <- trainSOM(x.data=iris[,1:4])
# choose the number of super-clusters
sc <- superClass(my.som)
plot(sc)
# cut the clustering
sc <- superClass(my.som, k=4)
summary(sc)
plot(sc)
plot(sc, type="hitmap", plot.legend=TRUE)
}

\keyword{classes}