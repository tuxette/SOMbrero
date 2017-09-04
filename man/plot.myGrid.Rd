\name{plot.myGrid}
\alias{plot.myGrid}

\title{
Draw a \code{myGrid} class object
}
\description{
Draw a grid corresponding to a \code{myGrid} class object in a graphical window.
}
\usage{
\method{plot}{myGrid}(x, neuron.col="white", \dots)
}
\arguments{
  \item{x}{The \code{myGrid} class object to be drawn.}
  \item{neuron.col}{Color(s) used to depict the neurons. Default value is 
  \code{white}. If the argument is composed of one single color, neurons will 
  all be filled with the same color. If the argument is composed of many colors, 
  the number of colors must match the total number of neurons.}
  \item{\dots}{Further arguments to the \code{\link{plot}} function.}
}
\details{
Color filling process uses the coordinates of the object \code{x}, having class
\code{myGrid} and included in \code{x$coord}.
}
\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}
\seealso{\code{\link{initGrid}} to define a \code{myGrid} class object.}
\examples{
# creating grid
a.grid <- initGrid(dimension=c(5,5), topo="square", dist.type="maximum")

# plotting grid
# without any color specification
plot(a.grid)
# generating colors from rainbow() function
my.colors <- rainbow(5*5)
plot(a.grid, neuron.col=my.colors)
}
\keyword{aplot}
