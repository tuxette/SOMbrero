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
Version: \tab 1.2\cr
Date: \tab 2016-09-02\cr
License: \tab GPL (>= 2)
}
The version of the SOM algorithm implemented in this package is the stochastic
version.

Several variants able to handle non-vectorial data are also implemented in their
stochastic versions: \code{type="korresp"} for contingency tables, as described
in Cottrell et al., 2004 (with the observation weights defined in Cottrel and
Letremy, 2005) and \code{type="relational"} for dissimilarity data, as described
in Olteanu and Villa-Vialaneix, 2015a with the fast implementation of Mariette
\emph{et al.}, 2017. A special focus has been put on representing graphs, as 
described in Olteanu and Villa-Vialaneix, 2015b.
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Julien Boelaert \email{julien.boelaert@gmail.com}\cr
Jerome Mariette \email{jerome.mariette@inra.fr}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Fabrice Rossi \email{fabrice.rossi@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}\cr \cr
Maintainer: Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}

\references{         
Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg:
Springer-Verlag, 3rd edition.

Cottrell M., Ibbou S., Letremy P. (2004) SOM-based algorithms for qualitative
variables. \emph{Neural Networks}, \strong{17}, 1149-1167.

Cottrell M., Letremy P. (2005) How to use the Kohonen algorithm to 
simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
\strong{21}, 119-138.

Letremy P. (2005) Programmes bases sur l'algorithme de Kohonen et dedies a
l'analyse des donnees. SAS/IML programs for 'korresp'.
\url{http://samos.univ-paris1.fr/Programmes-bases-sur-l-algorithme}.

Mariette J., Rossi F., Olteanu M., Mariette J. (2017) Accelerating stochastic 
kernel SOM. In: M. Verleysen, \emph{XXVth European Symposium on Artificial 
Neural Networks, Computational Intelligence and Machine Learning (ESANN 2017)},
i6doc, Bruges, Belgium, 269-274.

Olteanu M., Villa-Vialaneix N. (2015a) On-line relational and multiple 
relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 

Olteanu M., Villa-Vialaneix N. (2015b) Using SOMbrero for clustering and 
visualizing graphs. \emph{Journal de la Societe Francaise de Statistique},
\strong{156}, 95-119.

Rossi F. (2013) yasomi: Yet Another Self-Organising Map Implementation. R 
package, version 0.3. \url{https://github.com/fabrice-rossi/yasomi}

Villa-Vialaneix N. (2017) Stochastic self-organizing map variants with the R
package SOMbrero. In: J.C. Lamirel, M. Cottrell, M. Olteanu, \emph{12th 
International Workshop on Self-Organizing Maps and Learning Vector Quantization,
Clustering and Data Visualization (Proceedings of WSOM 2017)}, IEEE, Nancy,
France.
}

\seealso{
\code{\link{initGrid}}, \code{\link{trainSOM}}, \code{\link{plot.somRes}} and
\code{\link{sombreroGUI}}.
}
