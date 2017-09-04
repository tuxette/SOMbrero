\name{plot.somRes}
\alias{plot.somRes}

\title{
Draw a \code{somRes} class object
}
\description{
Produce graphics to help interpreting a \code{somRes} object.
}
\usage{
\method{plot}{somRes}(x, what=c("obs", "prototypes", "energy", "add"), 
     type=switch(what, "obs"="hitmap", "prototypes"="color", "add"="pie",
                 "energy"=NULL),
     variable = if (what=="add") NULL else if (type=="boxplot") 
       1:min(5,ncol(x$data)) else 1,
     my.palette=NULL,
     is.scaled = if (x$parameters$type=="numeric") TRUE else FALSE, 
     print.title=FALSE, the.titles=if (what!="energy") 
       switch(type, "graph"=1:prod(x$parameters$the.grid$dim),
              paste("Cluster", 1:prod(x$parameters$the.grid$dim))),
     proportional=TRUE, s.radius=1, pie.graph=FALSE, pie.variable=NULL, 
     view = if (x$parameters$type=="korresp") "r" else NULL, \ldots)
}

\arguments{
  \item{x}{A \code{somRes} class object.}
  \item{what}{What you want to plot. Either the observations (\code{obs}, 
  default case), the evolution of energy (\code{energy}), the prototypes 
  (\code{prototypes}) or an additional variable (\code{add}).}
  \item{type}{Further argument indicating which type of chart you want to have.
  Choices depend on the value of \code{what} (\code{what="energy"} has no
  \code{type} argument). Default values are \code{"hitmap"} for \code{obs}, 
  \code{"color"} for \code{prototypes} and \code{"pie"} for \code{add}. See
  section ``Details'' below for further details.}
  \item{variable}{Either the variable to be used for \code{what="add"} or the 
  index of the variable of the data set to consider. For \code{type="boxplot"}, 
  the default value is the sequence from 1 to the minimum between 5 and the 
  number of columns of the data set. In all other cases, default value is 1. 
  See \code{\link{somRes.plotting}} for further details.}
  \item{my.palette}{A vector of colors. If omitted, predefined palettes are
  used, depending on the plot case. This argument is used for the following
  combinations: all \code{"color"} types and \code{"prototypes"/"poly.dist"}.}
  \item{is.scaled}{A boolean indicating whether values should be scaled prior to
  plotting or not. Default value is \code{TRUE} when \code{type="numeric"} and 
  \code{FALSE} in the other cases.}
  \item{print.title}{Boolean used to indicate whether each neuron should have a 
  title or not. Default value is \code{FALSE}. It is feasible on the following 
  cases: all \code{"color"} types, all \code{"lines"} types, all 
  \code{"barplot"} types, all \code{"radar"} types, all \code{"boxplot"} types, 
  all \code{"names"} types, \code{"add"/"pie"}, \code{"prototypes"/"umatrix"}, 
  \code{"prototypes"/"poly.dist"} and \code{"add"/"words"}.}
  \item{the.titles}{The titles to be printed for each neuron if 
  \code{print.title=TRUE}. Default to a number which identifies the neuron.}
  \item{proportional}{Boolean used when \code{what="add"} and \code{type="pie"}. 
  It indicates if the pies should be proportional to the number of observations 
  in the class. Default value is \code{TRUE}.}
  \item{s.radius}{The size of the pies to be plotted (maximum size when 
  \code{proportional=TRUE}). The default value is \code{0.9}.}
  \item{pie.graph}{Boolean used when \code{what="add"} and \code{type="graph"}. 
  It indicates if the vertices should be pies or not.}
  \item{pie.variable}{The variable needed to plot the pies when 
  \code{what="add"}, \code{type="graph"} and argument \code{pie.graph=TRUE}.}
  \item{view}{Used only when the algorithm's type is \code{"korresp"}. It
  indicates whether rows (\code{"r"}) or columns (\code{"c"}) must be drawn.}
  \item{\dots}{Further arguments to be passed to the underlined plot function
  (which can be \code{\link{plot}}, \code{\link{barplot}}, \code{\link{pie}}...
  depending on \code{type}; see \code{\link{somRes.plotting}} for further
  details).}
}

\details{
See \code{\link{somRes.plotting}} for further details.
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}

\seealso{
\code{\link{trainSOM}} to run the SOM algorithm, that returns a \code{somRes} 
class object.
}

\keyword{aplot}