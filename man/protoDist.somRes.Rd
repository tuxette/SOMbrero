\name{protoDist}
\alias{protoDist.somRes}
\alias{protoDist}

\title{
Compute distances between prototypes
}

\description{Compute distances, either between all prototypes 
(\code{mode="complete"}) or only between prototypes' neighbours 
(\code{mode="neighbors")}.}

\usage{
\method{protoDist}{somRes}(object, mode=c("complete","neighbors"), \ldots)
}


\arguments{
  \item{object}{a \code{somRes} object.}
  \item{mode}{Specifies which distances should be computed.}
  \item{\ldots}{Not used.}
}
\details{
When \code{mode="complete"}, distances between all prototypes are computed. 
When \code{mode="neighbors"}, distances are computed only between the 
prototypes and their neighbors.
}

\value{
When \code{mode="complete"}, the function returns a square matrix which 
dimensions are equal to the product of the grid dimensions.

When \code{mode="neighbors"}, the function returns a list which length is equal 
to the product of the grid dimensions; the length of each item is equal to the 
number of neighbors.
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@univ-paris1.fr}
}

\seealso{\code{\link{trainSOM}}}

\examples{
set.seed(2343)
my.som <- trainSOM(x.data=iris[,1:4], dimension=c(5,5))
protoDist(my.som)
}

\keyword{methods}
