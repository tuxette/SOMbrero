\name{lesmis}
\alias{lesmis}
\alias{dissim.lesmis}
\docType{data}
\title{Dataset "Les Miserables"}

\description{This dataset contains the coappearance network (igraph object) of
characters in the novel Les Miserables (written by the French writter Victor
Hugo).}

\usage{data(lesmis)}

\format{
\code{lesmis} is an \code{\link[igraph]{igraph}} object. Its vertices are 
the characters of the novel and an edge indicates that the two characters appear
together in the same chapter of the novel, at least once. Vertex attributes for
this graph are \option{id}, a vertex number between 1 and 77, and
\option{label}, the character's name. The edge attribute \option{value} gives 
the number of co-appearances between the two characters afferent to the edge
(the \code{\link[igraph]{igraph}} can thus be made a weighted graph using this
attribute). Finally, a graph attribute \option{layout} is used to provide a 
layout (generated with the \code{\link[igraph]{igraph}} function 
\code{\link[igraph:layout.fruchterman.reingold]{layout_with_fr}})
for visualizing the graph.

\code{dissim.lesmis} is a dissimilarity matrix computed with the function 
\code{\link[igraph]{shortest_paths}} and containing the length of the shortest
paths between pairs of nodes.
}

\details{Les Miserables is a French historical novel, written by Victor Hugo 
and published in 1862. The co-appearance network has been extracted by D.E.
Knuth (1993).}

\references{
Hugo V. (1862) \emph{Les Miserables}.

Knuth D.E. (1993) \emph{The Stanford GraphBase: A Platform for Combinatorial 
Computing}. Reading (MA): Addison-Wesley.
}

\examples{
data(lesmis)
\dontrun{
summary(lesmis)
plot(lesmis,vertex.size=0)}
}

\keyword{datasets}
