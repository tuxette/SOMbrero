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
in Cottrell et al., 2004 (with weights as in Cottrel and Letremy, 2005); 
\code{type="relational"} for dissimilarity matrices, as described in Olteanu et 
al., 2015, with the fast implementation introduced in Mariette \emph{et al.}, 
2017.

\code{summary} produces a complete summary of the results that displays the 
parameters of the SOM, quality criteria and ANOVA. For \code{type="numeric"}
the ANOVA is performed for each input variable and test the difference of this
variable accross the clsuters of the map. For \code{type="relational"} a 
dissimilarity ANOVA is performed (see (Anderson, 2001), except that in the 
present version, a crude estimate of the p-value is used which is based on the
Fisher distribution and not on a permutation test.
}

\references{
Anderson M.J. (2001). A new method for non-parametric multivariate analysis of
variance. \emph{Austral Ecology}, \strong{26}, 32-46. 

Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg:
Springer-Verlag, 3rd edition.

Cottrell M., Ibbou S., Letremy P. (2004) SOM-based algorithms for qualitative
variables. \emph{Neural Networks}, \strong{17}, 1149-1167.

Cottrell M., Letremy P. (2005) How to use the Kohonen algorithm to 
simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
\strong{21}, 119-138.

Olteanu M., Villa-Vialaneix N. (2015) On-line relational and multiple
relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 

Mariette J., Rossi F., Olteanu M., Mariette J. (2017) Accelerating stochastic 
kernel SOM. In: M. Verleysen, \emph{XXVth European Symposium on Artificial 
Neural Networks, Computational Intelligence and Machine Learning (ESANN 2017)},
i6doc, Bruges, Belgium, 269-274.
}

\author{Jerome Mariette \email{jerome.mariette@inra.fr}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Fabrice Rossi \email{fabrice.rossi@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}

\note{Warning! Recording intermediate backups with the argument \code{nb.save}
can strongly increase the computational time since calculating the entire
clustering and the energy is time consumming. Use this option with care and only
when it is strictly necessary.}

\seealso{See \code{\link{initSOM}} for a description of the paramaters to pass
to the trainSOM function to change its behavior and \code{\link{plot.somRes}}
to plot the outputs of the algorithm.}

\examples{
# Run trainSOM algorithm on the iris data with 500 iterations
iris.som <- trainSOM(x.data=iris[,1:4])
iris.som
summary(iris.som)
}
\keyword{methods}
