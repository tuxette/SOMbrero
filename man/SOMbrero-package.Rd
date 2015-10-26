\name{SOMbrero}
\alias{SOMbrero}

\docType{package}
\title{
Self Organizing Maps Bound to Realize Euclidean and Relational Outputs
}
\description{
This package implements the stochastic (also called on-line) Self-Organising Map 
(SOM) algorithm for numeric and relational data.

It is based on a grid (see \code{\link{initGrid}}) which is part of the 
parameters given to the algorithm (see \code{\link{initSOM}} and 
\code{\link{trainSOM}}). Many graphs can help you with the results (see 
\code{\link{plot.somRes}}).
}
\details{
\tabular{ll}{
Package: \tab SOMbrero\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-27\cr
License: \tab GPL (>= 2)\cr
Shiny online interface: \url{http://shiny.nathalievilla.org}
}
The version of the SOM algorithm implemented in this package is the stochastic
version.

Several variants able to handle non-vectorial data are also implemented in their
stochastic versions: \code{type="korresp"} for contingency tables, as described
in Cottrel et al., 1993 (with the observation weights defined in Cottrel and
Letremy, 2005) and \code{type="relational"} for dissimilarity data, as described
in Olteanu and Villa-Vialaneix, 2015a. A special focus has been put on 
representing graphs, as described in Olteanu and Villa-Vialaneix, 2015b.
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}

Julien Boelaert \email{julien.boelaert@gmail.com}

Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}

Nathalie Villa-Vialaneix \email{nathalie.villa@toulouse.inra.fr}

Maintainer: Nathalie Villa-Vialaneix \email{nathalie.villa@toulouse.inra.fr}
}
\references{         
Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg:
Springer-Verlag, 3rd edition.

Cottrell, M., Letremy, P., Roy, E. (1993) Analyzing a contingency table with
Kohonen maps: a Factorial Correspondence Analysis. In: \emph{Proceedings of
IWANN'93, J. Cabestany, J. Mary, A. Prieto (Eds.), Lecture Notes in Computer
Science}, Springer-Verlag, 305--311.

Cottrell, M., Letremy, P. (2005) How to use the Kohonen algorithm to 
simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
\strong{21}, 119--138.

Letremy, P. (2005) Programmes bases sur l'algorithme de Kohonen et dedies a
l'analyse des donnees. SAS/IML programs for 'korresp'.
\url{http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme}.

Olteanu, M., Villa-Vialaneix, N. (2015a) On-line relational and multiple
relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 

Olteanu, M., Villa-Vialaneix, N. (2015b) Using SOMbrero for clustering and 
visualizing graphs. \emph{Journal de la Societe Francaise de Statistique}.
\emph{Under revision}.

Rossi, F. (2013) yasomi: Yet Another Self-Organising Map Implementation. R 
package, version 0.3. \url{https://github.com/fabrice-rossi/yasomi}
}
\seealso{
\code{\link{initGrid}}, \code{\link{trainSOM}}, \code{\link{plot.somRes}} and
\code{\link{sombreroGUI}}.
}
