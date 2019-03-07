\name{initGrid}
\alias{initGrid}
\alias{print.myGrid}
\alias{summary.myGrid}

\title{
Create an empty Self-Organizing Map structure
}
\description{
Create an empty Self-Organizing Map structure which is a list of 2-dimensional
points.
}
\usage{
initGrid(dimension=c(5,5), topo=c("square"), 
         dist.type=c("euclidean", "maximum", "manhattan", "canberra", "binary",
                     "minkowski", "letremy"))
\method{print}{myGrid}(x, ...)
\method{summary}{myGrid}(object, ...)
}
\arguments{
  \item{dimension}{Vector of two integer points corresponding to the x 
  dimension and the y dimension. Default values are: \code{(5,5)}.}
  \item{topo}{The topology to be used to build the grid. Default value is 
  \code{square}.}
  \item{dist.type}{The distance type to be used. Default value is 
  \code{euclidean}. Type 'letremy' corresponds to the original implementation by 
  Patrick Letremy.}
  \item{x, object}{an object of class \code{myGrid}}
  \item{...}{not used}
}
\value{
\code{initGrid} function returns an object of class \code{myGrid}, including 
the following components:
\item{coord}{A two columns matrix which provides the coordinates of the points 
of the Self-Organizing Map structure,}
\item{topo}{Same value as the arguments given to \code{initGrid} function,}
\item{dim}{Same values as the arguments given to \code{initGrid} function.}
\item{dist.type}{Same value as the arguments given to \code{initGrid} function.}
}
\author{Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Vialaneix \email{nathalie.vialaneix@inra.fr}
}
\references{
Letremy P. (2005) Programmes bases sur l'algorithme de Kohonen et dedies a
l'analyse des donnees. SAS/IML programs for 'korresp'.
\url{http://samm.univ-paris1.fr/Programmes-SAS-de-cartes-auto}.
}
\seealso{See \code{\link{plot.myGrid}} for plotting a \code{myGrid} class object
in a graphical window.}
\examples{
# creating a default grid
# default parameters are: 5x5 dimension, squared topology 
# and letremy distance type
initGrid()

# creating a 5x7 squared grid
initGrid(dimension=c(5, 7), topo="square", dist.type="maximum")
}
\keyword{classes}
