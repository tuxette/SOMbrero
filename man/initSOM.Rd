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
        dist.type=c("letremy", "maximum", "euclidean", "manhattan",
                    "canberra", "binary", "minkowski"),
        type=c("numeric", "relational", "korresp"), mode=c("online"), 
        maxit=500, nb.save=0, verbose=FALSE, proto0=NULL, 
        init.proto=switch(type, 
                          "numeric"="random", 
                          "relational"="obs",
                          "korresp"="random"),
        scaling=switch(type,
                       "numeric"="unitvar",
                       "relational"="none",
                       "korresp"="chi2"), 
        radius.type=c("letremy"), eps0=1)
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
  \item{dist.type}{The neighborhood relationship on the grid. Default value is
  \code{"letremy"} which is the original implementation by Patrick Letremy.
  The other possible values are the values passed to \code{method} in
  function \code{\link[stats]{dist}}.}
  \item{type}{The SOM algorithm type. Possible values are: \code{numeric} 
  (default value), \code{korresp} and \code{relational}.}
  \item{mode}{The SOM algorithm mode. Default value is \code{online}.}
  \item{maxit}{The maximum number of iterations to be done during the SOM 
  algorithm process. Default value is \code{500}. Other data-driven defaults
  are set by function \code{trainSOM}.}
  \item{nb.save}{The number of intermediate back-ups to be done during the 
  algorithm process. Default value is \code{0}.}
  \item{verbose}{The boolean value which activates the verbose mode during the
  SOM algorithm process. Default value is \code{FALSE}.}
  \item{proto0}{The initial prototypes. Default value is \code{NULL}.}
  \item{init.proto}{The method to be used to initialize the prototypes, either 
  randomization or an observation sample. This 
  argument is only considered when \code{proto0=NULL}. Default value is 
  \code{random} for the \code{numeric} and \code{korresp} types, and \code{obs}
  for the \code{relational} type.}
  \item{scaling}{The type of data pre-processing. For \code{numeric} SOM, 
  possibilities are \code{unitvar} (data are centered and scaled; this 
  is the default value for a  \code{numeric} SOM) and \code{none} (no 
  pre-processing and this is the only value allowed for a \code{relational}
  SOM), 
  \code{center} (data are centered but not scaled). For \code{korresp} SOM, the 
  only available value (and default) is \code{chi2}.}
  \item{radius.type}{The neighbourhood type. Default value is \code{"letremy"},
  which corresponds to the original implementation by Patrick Letremy.}
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
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa@univ-paris1.fr}
}
\references{
Letremy, P. (2005) Programmes bases sur l'algorithme de Kohonen et dedies a
l'analyse des donnees. SAS/IML programs for 'korresp'.
\url{http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme}.
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
