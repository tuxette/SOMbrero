\name{somRes.plotting}
\alias{somRes.plotting}

\title{Plotting \code{somRes} results}

\description{
Useful details on how to produce graphics to help interpreting a \code{somRes}
object.
Important: all these graphics are available when the algorithm's type is
\code{"numeric"} ; those which are available for a \code{korresp} SOM are 
marked by a * and those which are available for a \code{relational} SOM are 
marked with a #.
}

\section{Graphics on the observations: \code{what="obs"}}{
The possible values for \code{type} are: \code{"hitmap"}(*, #), \code{"color"},
\code{"lines"}, \code{"barplot"}, \code{"names"}(*, #), \code{"boxplot"} and
\code{"radar"}.

For the cases \code{what="obs"} and \code{what="add"}, if a neuron is empty,
nothing will be plotted at its location.

\itemize{
\item \code{"hitmap"} (*, #) plots proportional areas according to the number of
observations per neuron. It is the default plot when \code{what="obs"}.
\item \code{"color"} can have two more arguments, \code{var}, the index of the  
variable to be considered (default, \code{1}), and \code{my.palette} for the 
colors to be used. Neurons are filled using the given colors according to the 
average value level of the observations for the chosen variable.
\item \code{"lines"} plots, for each neuron, the average value level of the
observations, with lines. One point represents a variable. All variables of the
dataset used to train the algorithm are plotted.
\item \code{"barplot"} is similar to \code{"lines"} but using barplots. Then, 
a bar represents a variable.
\item \code{"radar"} is similar to \code{"lines"} but using radars. Then, a 
slice represents a variable. If needed, a legend can be added ; its location 
has to be passed by the \code{key.loc} argument (see \code{stars}).
\item \code{"names"} (*, #) prints on the grid the element names (i.e., the
names of the rows) in the neuron to which it belongs.
\item \code{"boxplot"} plots boxplots for several observations in every neuron.
This case can handle 5 variables at most. The default behavior is to plot the
boxplots forthe first 5 variables of the data set; if there is less than 5
variables in the data set, they will all be plotted.}

When the algorithm's type is \code{korresp} or \code{relational}, only the types
\code{"hitmap"} and \code{"names"} are available.
}

\section{Graphic on the energy: \code{what="energy"} (*, #)}{
This graphic is only available if some intermediate backups have been
registered (i.e., \code{x$parameters$nb.save>1}). Graphic plots the evolution of
the level of the energy according to the registered steps.}

\section{Graphics on the prototypes: \code{what="prototypes"}}{
The possible values for \code{type} are: \code{"3d"}(*), \code{"lines"}(*, #),
\code{"barplot"}(*, #), \code{"radar"}(*, #), \code{"color"}(*), 
\code{"smooth.dist"} (*, #), \code{"poly.dist"}(*, #), \code{"umatrix"}(*, #), 
\code{"mds"}(*, #) and \code{"grid.dist"}(*, #). 

\itemize{
\item \code{"lines"} (*, #) has the same behavior as the \code{"lines"} case 
described in the observations section, but according to the prototypes level;
\item \code{"barplot"} (*, #) has the same behavior as the \code{"barplot"} case 
described in the observations section, but according to the prototypes level;
\item \code{"radar"} (*, #) has the same behavior as the \code{"radar"} case 
described in the observations section, but according to the prototypes level;
\item \code{"color"} (*) has the same behavior as the \code{"color"} case 
described in the observations section, but according to the prototypes level;
\item \code{"3d"} case is similar to the \code{"color"} case, but in 3
dimensions, with x and y the coordinates of the grid and z the value of the
prototypes for the considered variable;
\item \code{"smooth.dist"} (*, #) depicts the average distance between a 
prototypes and its neighbors on a map where x and y are the coordinates of the 
prototypes on the grid;
\item \code{"poly.dist"} (*, #) also represents the distances between 
prototypes but with polygons plotted for each neuron. The closest from the 
border the polygon point is, the closest the pairs of prototypes are. The color 
used for filling the polygon shows the number of observations in each neuron. 
A white polygon means that there is no observation. With the default colors, a 
red polygon means a high number of observations;
\item \code{"umatrix"} (*, #) is another way of plotting distances between 
prototypes. The grid is plotted and filled with \code{my.palette} colors 
according to the mean distance between the current neuron and the neighboring 
neurons. With the default colors, red indicates proximity.
\item \code{"mds"} (*, #) plots the number of the neuron on a map according to 
a Multi Dimensional Scaling (MDS) projection on a two dimensional space.
\item \code{"grid.dist"} (*, #) plots on a 2 dimension map all distances. The 
number of points on this picture is equal to: 
\eqn{\frac{\textrm{number of neurons}\times(\textrm{number of neurons}-1)}{2}}{%
number of neurons  * (number of neurons - 1) / 2}. 
On the x axis corresponds to the prototype distances whereas the y axis depicts 
the grid distances.
}
}

\section{Graphics on an additional variable: \code{what="add"} (#)}{
The case \code{what="add"} considers an additional variable, which has to be 
given to the argument \code{variable}. Its length must match the number of
observations in the original data. Then the possible values for \code{type} are:
\code{"pie"}(#), \code{"color"}(#), \code{"lines"}(#), \code{"boxplot"}(#),
\code{"barplot"}(#), \code{"radar"}(#), \code{"names"}(#), \code{"words"}(#) and
\code{"graph"}(#). 

\itemize{
\item \code{"color"} (#) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be a 
numerical vector;
\item \code{"lines"} (#) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be a 
numerical matrix or a data frame;
\item \code{"boxplot"} (#) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be 
either a numeric vector or a numeric matrix/data frame;
\item \code{"barplot"} (#) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be 
either a numeric vector or a numeric matrix/data frame;
\item \code{"radar"} (#) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be a 
numerical matrix or data frame;
\item \code{"pie"} requires the argument \code{variable} to be a factor vector
and plots one pie for each neuron according to this factor;
\item \code{"names"} (#) has the same behavior as the \code{"names"} case 
described in the observations section. Then, the names to be printed are the 
elements of the variable given to the \code{variable} argument;
\item \code{"words"} (#) needs the argument \code{variable} be a contingency
table: names of the columns will be used as words and the values express the 
frequency of a given word in the observation. Then, for each neuron of the grid,
the words will be printed with sizes proportional to their frequency in the
neuron;
\item Last option is \code{"graph"} (#). The argument \code{variable} must be an
\code{igraph} object (see \code{library(igraph)}. According to the existing
edges in the graph and to the clustering obtained with the SOM algorithm, a 
clustered graph will be produced where a vertex between two vertices represents
a neuron and the width of an edge is proportional to the number of edges in the
given graph between the vertices affected to the corresponding neurons. The
option can handle two more arguments: \code{pie.graph} and \code{pie.variable}.
These are used to display the vertex as pie charts. For this case, 
\code{pie.graph} must be set to \code{TRUE} and a factor vector is supplied by
\code{pie.variable}.}

When the algorithm's type is \code{korresp}, no graphic is available for
\code{what="add"}.

All these graphics are available for a \code{relational} SOM.}

\section{Further arguments via \dots}{
Further arguments, their reference functions and the \code{plot.somRes} cases 
are summarized in the following list:
\itemize{
\item \code{\link{plot}} is called by the cases:
\itemize{
\item \code{what="energy"}
\item \code{type="lines"}
\item \code{what="prototypes"/type="mds"}
}
\item \code{\link{plot.myGrid}} is called by the cases:
\itemize{
\item \code{what="obs"/type="hitmap"}
\item \code{type="color"}
\item \code{what="prototypes"/type="poly.dist"}
\item \code{what="prototypes"/type="umatrix"}
}
\item \code{\link[igraph]{plot.igraph}} is called by the case 
\code{what="add"/type="graph"}
\item \code{\link{pie}} is called by the case \code{what="add"/type="pie"}
\item \code{\link{barplot}} is called by the cases \code{type="barplot"}
\item \code{\link{boxplot}} is called by the cases \code{type="boxplot"}
\item \code{\link{stars}} is called by the cases \code{type="radar"}
\item \code{\link{persp}} is called by the case 
\code{what="prototypes"/type="3d"}
\item \code{\link[wordcloud]{wordcloud}} is called by the cases:
\itemize{
\item \code{type="names"}
\item \code{what="add"/type="words"}
}
}
}

\author{Laura Bendhaiba \email{laurabendhaiba@gmail.com}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Villa-Vialaneix \email{nathalie.villa-vialaneix@inra.fr}
}

\examples{
# run the SOM algorithm on the numerical data of 'iris' data set
iris.som <- trainSOM(x.data=iris[,1:4], nb.save=2)

# plots
# on energy
plot(iris.som, what="energy") # energy
# on prototypes
plot(iris.som, what="prototypes", type="3d", variable="Sepal.Length")
# on an additional variable: the flower species
plot(iris.som, what="add", type="pie", variable=iris$Species)
}
