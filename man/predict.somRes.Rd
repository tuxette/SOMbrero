\name{predict.somRes}
\alias{predict.somRes}

\title{
Predict the classification of a new observation
}

\description{Predict the neuron where a new observation is classified}

\usage{
\method{predict}{somRes}(object, x.new=NULL, ..., tolerance=10^(-10))
}

\arguments{
\item{object}{a \code{somRes} object}
\item{x.new}{a new observation (optional). Default values is NULL which
corresponds to performing prediction on the training dataset}
\item{...}{not used}
\item{tolerance}{numeric tolerance (to avoid numeric instability during 'cosine'
pre-processing). Default value is \option{10^(-10)}}
}

\details{The number of columns of the new observations (or its length if only 
one observation is provided) must match the number of colums of the data set
given to the SOM algorithm (see \code{\link{trainSOM}}).}

\value{\code{predict.somRes} returns the number of the neuron to which the new 
observation is assigned (i.e., neuron with the closest prototype).

When the algorithm's type is \code{"korresp"}, \code{x.new} must be the original 
contingency table passed to the algorithm.}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Julien Boelaert \email{julien.boelaert@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@toulouse.inra.fr}
}

\seealso{\code{\link{trainSOM}}}

\examples{
set.seed(2343)
my.som <- trainSOM(x.data=iris[-100,1:4], dimension=c(5,5))
predict(my.som, iris[100,1:4])
}

\keyword{cluster}