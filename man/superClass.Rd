\name{superClass}
\alias{superClass}
\alias{superClass.somRes}
\alias{print.somSC}
\alias{summary.somSC}
\alias{plot.somSC}

\title{Create super-clusters from SOM clustering}

\description{
Aggregate the resulting clustering of the SOM algorithm into super-clusters.}

\usage{
\method{superClass}{somRes}(sommap, method="ward", members=NULL, k=NULL, 
h=NULL, \ldots)
\method{print}{somSC}(x, \ldots)
\method{summary}{somSC}(object, \ldots)
\method{plot}{somSC}(x, type=c("dendrogram", "grid", "hitmap", "lines", 
                                 "barplot", "boxplot", "mds", "color", 
                                 "poly.dist", "pie", "graph", "dendro3d", 
                                 "radar"),
                       plot.var=TRUE, plot.legend=FALSE, add.type=FALSE, 
                       \ldots)
}

\arguments{
  \item{sommap}{A \code{somRes} object}
  \item{method, members}{Arguments passed to the \code{\link{hclust}} function.}
  \item{k, h}{Arguments passed to the \code{\link{cutree}} function
  (respectively, the number of super-clusters or the height where to cut the
  dendrogram).}
  \item{x, object}{A \code{somSC} object}
  \item{type}{The type of plot to draw. Default value is \code{"dendrogram"}, 
  to plot the dendrogram of the clustering. Case \code{"grid"} plots the grid 
  in color according to the super clustering. All other cases are those
  available in the function \code{\link{plot.somRes}}.}
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

The different super classes are identified in the following ways:
\itemize{
\item either with different color, when \code{type} is set among: 
\code{"grid"} (*, #), \code{"hitmap"} (*, #), \code{"lines"} (*, #), 
\code{"barplot"} (*, #), \code{"boxplot"}, \code{"mds"} (*, #), 
\code{"dendro3d"} (*, #), \code{"graph"} (*, #)
\item or with title, when \code{type} is set among: \code{"color"} (*), 
\code{"poly.dist"} (*, #), \code{"pie"} (#), \code{"radar"} (#)
}
In the list above, graphics available for a \code{korresp} SOM are marked with 
a * whereas those available for a \code{relational} SOM are marked with a #.
}

\value{The \code{superClass} function returns an object of class \code{somSC} 
which is a list of the following elements:
\item{cluster}{The super clustering of the prototypes (only if either \code{k} 
or \code{h} are given by user).}
\item{tree}{An \code{\link{hclust}} object.}
\item{som}{The \code{somRes} object given as argument (see 
\code{\link{trainSOM}} for details).}
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@univ-paris1.fr}
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