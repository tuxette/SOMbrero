\name{trainSOM}
\alias{trainSOM}
\alias{summary.somRes}
\alias{print.somRes}

\title{
Run the SOM algorithm
}
\description{
The \code{trainSOM} function returns a \code{somRes} class object which 
contains the outputs of the algorithm.
}
\usage{
trainSOM(x.data, \ldots)
\method{print}{somRes}(x, \ldots)
\method{summary}{somRes}(object, \ldots)
}

\arguments{
  \item{x.data}{a data frame or matrix containing the observations to be mapped 
  on the grid by the SOM algorithm.}
  \item{...}{Further arguments to be passed to the function
  \code{\link{initSOM}} for specifying the parameters of the algorithm. The 
  default values of the arguments \code{maxit} and \code{dimension} are 
  calculated according to the SOM type if the user does not set them:
  \itemize{
    \item \code{maxit} is equal to (number of rows+number of columns)*5 if the 
    SOM type is \code{korresp}. It is equal to number of rows*5 in all other 
    SOM types
    \item \code{dimension}: for a \code{korresp} SOM, is approximately equal to
    the square root of the number of observations to be classified divided by
    10 but it is never smaller than 5 or larger than 10.
  }}
  \item{x, object}{an object of class \code{somRes}}
}

\value{
The \code{trainSOM} function returns an object of class \code{somRes} which 
contains the following components:
\item{clustering}{the final classification of the data.}
\item{prototypes}{the final coordinates of the prototypes.}
\item{energy}{the final energy of the map.}
\item{backup}{a list containing some intermediate backups of the prototypes 
coordinates, clustering, energy and the indexes of the recorded backups, if
\code{nb.save} is set to a value larger than 1.}
\item{data}{the original dataset used to train the algorithm.}
\item{parameters}{a list of the map's parameters, which is an object of class
\code{paramSOM} as produced by the function \code{\link{initSOM}}.}
The function \code{summary.somRes} also provides an ANOVA (ANalysis Of VAriance)
of each input numeric variables in function of the map's clusters. This is 
helpful to see which variables participate to the clustering.
}
\details{
The version of the SOM algorithm implemented in this package is the stochastic
version.

Several variants able to handle non-vectorial data are also implemented in their
stochastic versions: \code{type="korresp"} for contingency tables, as described
in Cottrel et al., 1993; \code{type="relational"} for dissimilarity matrices, 
as described in Olteanu et al., 2013.
}
\references{
Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg:
Springer-Verlag, 3rd edition.

Cottrell, M., Letremy, P., Roy, E. (1993) Analyzing a contingency table with
Kohonen maps: a Factorial Correspondence Analysis. In: \emph{Proceedings of
IWANN'93, J. Cabestany, J. Mary, A. Prieto (Eds.), Lecture Notes in Computer
Science}, Springer-Verlag, 305--311.

Olteanu, M., Villa-Vialaneix, N. (2013) On-line relational and multiple
relational SOM. \emph{Neurocomputing}. Forthcoming.
}
\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@univ-paris1.fr}
}
\note{Warning! Recording intermediate backups with the argument \code{nb.save}
can strongly increase the computational time since calculating the entire
clustering and the energy is time consumming. Use this option with care and only
when it is strictly necessary.}

\seealso{See \link{plot.somRes} to plot the outputs of the algorithm.}

\examples{
# Run trainSOM algorithm on the iris data with 500 iterations
iris.som <- trainSOM(x.data=iris[,1:4])
iris.som
summary(iris.som)
}
\keyword{methods}
