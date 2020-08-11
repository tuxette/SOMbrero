## Documentation for datasets
## ----------------------------------------------------------------------------

## lesmis
###############################################################################
#' @title Dataset "Les Misérables"
#' @name lesmis
#' @aliases dissim.lesmis
#' 
#' @description This dataset contains the coappearance network (igraph object) 
#' of characters in the novel Les Misérables (written by the French writter 
#' Victor Hugo).
#' 
#' @docType data
#' 
#' @format \code{lesmis} is an \code{\link[igraph]{igraph}} object. Its vertices
#' are the characters of the novel and an edge indicates that the two characters
#' appear together in the same chapter of the novel, at least once. Vertex 
#' attributes for this graph are \option{id}, a vertex number between 1 and 77, 
#' and \option{label}, the character's name. The edge attribute \option{value} 
#' gives the number of co-appearances between the two characters afferent to the
#' edge (the \code{\link[igraph]{igraph}} can thus be made a weighted graph 
#' using this attribute). Finally, a graph attribute \option{layout} is used to 
#' provide a layout (generated with the \code{\link[igraph]{igraph}} function 
#' \code{\link[igraph:layout.fruchterman.reingold]{layout_with_fr}}) for 
#' visualizing the graph.
#' 
#' \code{dissim.lesmis} is a dissimilarity matrix computed with the function 
#' \code{\link[igraph]{shortest_paths}} and containing the length of the 
#' shortest paths between pairs of nodes.
#' 
#' @details Les Misérables is a French historical novel, written by Victor Hugo 
#' and published in 1862. The co-appearance network has been extracted by D.E.
#' Knuth (1993).
#' 
#' @references
#' Hugo V. (1862) \emph{Les Miserables}.
#' 
#' Knuth D.E. (1993) \emph{The Stanford GraphBase: A Platform for Combinatorial 
#' Computing}. Reading (MA): Addison-Wesley.
#' 
#' @examples 
#' data(lesmis)
#' \dontrun{
#' summary(lesmis)
#' plot(lesmis,vertex.size=0)}

NULL

## presidentielles2020
###############################################################################
#' @title 2002 French presidential election data set
#' @name presidentielles2002
#' 
#' @description This data set provides the number of votes at the first round of
#' the 2002 French presidential election for each of the 16 candidates for 106
#' administrative districts called "Départements". 
#' 
#' @docType data
#' 
#' @format \code{presidentielles2002} is a data frame of 106 rows (the French
#' administrative districts called "Departements") and 16 columns (the 
#' candidates).
#' 
#' @source The data are provided by the French ministry "Ministère de 
#' l'Intérieur". The original data can be downloaded at
#' \url{https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles} 
#' (2002 elections and "Résultats par départements").
#' 
#' @references The 2002 French presidential election consisted of two rounds. 
#' The second round attracted a greater than usual amount of international 
#' attention because of far-right candidate Le Pen's unexpected victory over 
#' Socialist candidate Lionel Jospin. The event is known because, on the one 
#' hand, the number of candidates was unusually high (16) and, on the other 
#' hand, because the polls had failed to predict that Jean-Marie Le Pen would be
#' on the second round.
#' 
#' Further comments at
#' \url{http://en.wikipedia.org/wiki/French_presidential_election,_2002}.
#' 
#' @examples 
#' data(presidentielles2002)
#' apply(presidentielles2002,2,sum)

NULL