\name{initSOM}
\alias{initSOM}
\alias{print.paramSOM}
\alias{summary.paramSOM}

\title{
Initialize parameters for the SOM algorithm
}
\description{
The \code{initSOM} function returns a \code{paramSOM} class object which
contains the parameters needed to run the SOM algorithm.
}
\usage{
initSOM(dimension=c(5,5), topo=c("square"), 
        radius.type=c("gaussian", "letremy"),
        dist.type=switch(match.arg(radius.type),
                         "letremy"="letremy", "gaussian"="euclidean"),
        type=c("numeric", "relational", "korresp"), mode=c("online"), 
        affectation=c("standard", "heskes"), maxit=500, nb.save=0,
        verbose=FALSE, proto0=NULL, 
        init.proto=switch(type, "numeric"="random", "relational"="obs",
                          "korresp"="random"),
        scaling=switch(type, "numeric"="unitvar", "relational"="none",
                       "korresp"="chi2"), eps0=1)
\method{print}{paramSOM}(x, \ldots)
\method{summary}{paramSOM}(object, \ldots)
}
\arguments{
  \item{dimension}{Vector of two integer points corresponding to the x 
  dimension and the y dimension of the \code{myGrid} class object. Default 
  values are: \code{(5,5)}. Other data-driven defaults are set by function 
  \code{trainSOM}.}
  \item{topo}{The topology to be used to build the grid of the \code{myGrid} 
  class object. Default value is \code{square}.}
  \item{radius.type}{The neighbourhood type. Default value is \code{"gaussian"},
  which corresponds to a Gaussian neighbourhood. The annealing of the 
  neighbourhood during the training step is similar to the one implemented in 
  \href{https://github.com/fabrice-rossi/yasomi}{yasomi}. The alternative value 
  corresponds to an piecewise linear neighbourhood as implementated by Patrick 
  Letremy in his SAS programs.}
   \item{dist.type}{The neighborhood relationship on the grid. When 
  \code{radius.type} is \code{letremy}, default value is \code{letremy} 
  which is the original implementation by Patrick Letremy. When 
  \code{radius.type} is \code{gaussian}, default value is \code{euclidean}. The
  other possible values (\code{maximum}, \code{manhattan}, \code{canberra},
  \code{binary}, \code{minkowski}) are passed to \code{method} in function
  \code{\link[stats]{dist}}. \code{dist.type="letremy"} is not permitted with
  \code{radius.type="gaussian"}.}
  \item{type}{The SOM algorithm type. Possible values are: \code{numeric} 
  (default value), \code{korresp} and \code{relational}.}
  \item{mode}{The SOM algorithm mode. Default value is \code{online}.}
  \item{affectation}{The SOM affectation type. Default value is \code{standard}
  which corresponds to a hard affectation. Alternative is \code{heskes} which
  corresponds to Heskes's soft affectation.}
  \item{maxit}{The maximum number of iterations to be done during the SOM 
  algorithm process. Default value is \code{500}. Other data-driven defaults
  are set by function \code{trainSOM}.}
  \item{nb.save}{The number of intermediate back-ups to be done during the 
  algorithm process. Default value is \code{0}.}
  \item{verbose}{The boolean value which activates the verbose mode during the
  SOM algorithm process. Default value is \code{FALSE}.}
  \item{proto0}{The initial prototypes. Default value is \code{NULL}.}
  \item{init.proto}{The method to be used to initialize the prototypes, which
  may be \code{random} (randomization), \code{obs} (each prototype is assigned
  a random observation) or \code{pca}. In \code{pca} the prototypes are 
  initialized to the observations closest to a grid along the two first 
  principal components of the data (\code{numeric} case) or along a
  two-dimensional multidimensional scaling (\code{relational} case, equivalent
  to a \code{relational} PCA). Default value is \code{random} for the
  \code{numeric} and \code{korresp} types, and \code{obs} for the
  \code{relational} type. \code{pca} is not available for \code{korresp} SOM.}
  \item{scaling}{The type of data pre-processing. For \code{numeric} SOM, 
  possibilities are \code{unitvar} (data are centered and scaled; this 
  is the default value for a \code{numeric} SOM), \code{none} (no 
  pre-processing), and \code{center} (data are centered but not scaled).
  For \code{korresp} SOM, the only available value is \code{chi2}.
  For \code{relational} SOM, possibilities are \code{none} (no pre-processing, 
  default value for \code{relational} SOM) and \code{cosine}. This last one 
  first turns the dissimilarity into a similarity using the suggestion in
  (Lee and Verleysen, 2007). Then, a cosine normalization as described in 
  (Ben-Hur and Weston, 2010) is applied to the kernel, that is finally turned
  back into its induced distance. For further details on this processing, have
  a look at the corresponding documentation in the directory "doc" of the
  package's installation directory.}
  \item{eps0}{The scaling value for the stochastic gradient descent step
  in the prototypes' update. The scaling value for the stochastic gradient
  descent step is equal to
  \eqn{\frac{0.3\epsilon_0}{1+0.2t/\textrm{dim}}}{0.3*eps0/(1+0.2*t/dim)} where
  \eqn{t}{t} is the current step number and \eqn{\textrm{dim}}{dim} is the grid
  dimension (width multiplied by height).}
  \item{x, object}{an object of class \code{paramSOM}.}
  \item{\ldots}{not used}
}
\value{
The \code{initSOM} function returns an object of class \code{paramSOM} which is
a list of the parameters passed to the \code{initSOM} function, plus the default
parameters for the ones not specified by the user.
}
\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Julien Boelaert \email{julien.boelaert@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@toulouse.inra.fr}
}
\references{
Ben-Hur A., Weston J. (2010) A user's guide to support vector machine. In:
\emph{Data Mining Techniques for the Life Sciences}, Springer-Verlag, 223-239.

Heskes T. (1999) Energy functions for self-organizing maps. In: \emph{Kohonen
Maps}, Oja E., Kaski S. (Eds.), Elsevier, 303-315.

Lee J., Verleysen M. (2007) \emph{Nonlinear Dimensionality Reduction}.
Information Science and Statistics series, Springer.

Letremy, P. (2005) Programmes bases sur l'algorithme de Kohonen et dedies a
l'analyse des donnees. SAS/IML programs for 'korresp'.
\url{http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme}.

Rossi, F. (2013) yasomi: Yet Another Self-Organising Map Implementation. R 
package, version 0.3. \url{https://github.com/fabrice-rossi/yasomi}
}
\seealso{
See \link{initGrid} for creating a SOM prior structure (grid).
}
\examples{
# create a default 'paramSOM' class object
default.paramSOM <- initSOM()
summary(default.paramSOM)
}
\keyword{classes}
