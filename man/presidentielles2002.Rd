\name{presidentielles2002}
\alias{presidentielles2002}
\docType{data}
\title{2002 French presidential election data set}
\description{
This data set provides the number of votes at the first round of the 2002 French
presidential election for each of the 16 candidates for 106 administrative
districts called "Departements". 
}
\usage{data(presidentielles2002)}
\format{
  \code{presidentielles2002} is a data frame of 106 rows (the French
  administrative districts called "Departements") and 16 columns
  (the candidates).
}
\source{
The data are provided by the French minister "Ministere de l'Interieur". The
original data can be downloaded at
\url{http://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles} (2002
elections and "Resultats par departements")
}
\references{
The 2002 French presidential election consisted of two rounds. The second round
attracted a greater than usual amount of international attention because of
far-right candidate Le Pen's unexpected victory over Socialist candidate Lionel
Jospin. The event is known because, on the one hand, the number of candidates 
was unusually high (16) and, on the other hand, because the polls had failed to
predict that Jean-Marie would be on the second round.

Further comments at
\url{http://en.wikipedia.org/wiki/French_presidential_election,_2002} or at
\url{https://bit.ly/2pQ6Igi} (in French).
}
\examples{
data(presidentielles2002)
apply(presidentielles2002,2,sum)
}
\keyword{datasets}
