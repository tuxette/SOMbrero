\name{somRes.plotting}
\alias{somRes.plotting}

\title{Plotting \code{somRes} results}

\description{
Useful details on how to produce graphics to help interpreting a \code{somRes}
object.\cr
Important: all these graphics are available when the algorithm's type is
\code{"numeric"} ; those which are available for a \code{korresp} SOM are 
marked by a K and those which are available for a \code{relational} SOM are 
marked with a R.
}

\section{Graphics on the observations: \code{what = "obs"}}{
The possible values for \code{type} are: \code{"hitmap"}(K, R), \code{"color"},
\code{"lines"}, \code{"barplot"}, \code{"names"}(K, R) and \code{"boxplot"}.

For the cases \code{what = "obs"} and \code{what = "add"}, if a neuron is empty,
nothing will be plotted at its location.

\itemize{
\item \code{"hitmap"} (K, R) plots proportional areas according to the number of
observations per neuron. It is the default plot when \code{what="obs"}.
\item \code{"color"} can have one more argument, \code{variable}, the name or 
index of the variable to be considered (default, \code{1}, the first variable). 
Neurons are filled using the given colors according to the average value level 
of the observations for the chosen variable.
\item \code{"meanline"} plots, for each neuron, the average value level of the
observations, with lines and points. One point represents a variable. By 
default, all variables of the dataset used to train the algorithm are plotted 
but a vector of variables (names or indexes) can be provided with the argument 
\code{variable}.
\item \code{"barplot"} is similar to \code{"lines"} but using barplots. Then, 
a bar represents a variable.
\item \code{"boxplot"} plots boxplots for the observations in every neuron, by 
variable. Like \code{"lines"} and \code{"barplot"} a vector of variables (names
or indexes) can be provided with the argument \code{variable}.
\item \code{"lines"} plots a line for each observation in every neuron, between 
variables. A vector of variables (names or indexes) can be provided with the 
argument \code{variable}.
\item \code{"names"} (K, R) prints on the grid the element names (i.e., the
row names or row and column names in the case of \code{korresp}) in the neuron
to which it belongs.}

When the algorithm's type is \code{korresp} or \code{relational}, only the types
\code{"hitmap"} and \code{"names"} are available.
}

\section{Graphic on the energy: \code{what = "energy"} (K, R)}{
This graphic is only available if some intermediate backups have been registered
(i.e., with the argument \code{nb.save} of \code{trainSOM} or \code{initSOM} 
resulting in \code{x$parameters$nb.save>1}). Graphic plots the evolution of the 
level of the energy according to the registered steps.}

\section{Graphics on the prototypes: \code{what = "prototypes"}}{
The possible values for \code{type} are: \code{"3d"}(K), \code{"lines"}(K, R),
\code{"barplot"}(K, R), \code{"color"}(K), \code{"smooth.dist"} (K, R), 
\code{"poly.dist"}(K, R), \code{"umatrix"}(K, R), \code{"mds"}(K, R) and 
\code{"grid.dist"}(K, R). 

\itemize{
\item \code{"lines"} (K, R) has the same behavior as the \code{"lines"} case 
described in the observations section, but according to the prototypes level;
\item \code{"barplot"} (K, R) has the same behavior as the \code{"barplot"} case 
described in the observations section, but according to the prototypes level;
\item \code{"color"} (K) has the same behavior as the \code{"color"} case 
described in the observations section, but according to the prototypes level;
\item \code{"3d"} case is similar to the \code{"color"} case, but in 3
dimensions, with x and y the coordinates of the grid and z the value of the
prototypes for the considered variable. This function can take two more 
arguments: \code{maxsize} (default to \code{2}) and \code{minsize} (default to 
\code{0.5}) for the size of the points representing neurons;
\item \code{"smooth.dist"} (K, R) depicts the average distance between a 
prototypes and its neighbors on a map where x and y are the coordinates of the 
prototypes on the grid;
\item \code{"poly.dist"} (K, R) also represents the distances between 
prototypes but with polygons plotted for each neuron. The closest from the 
border the polygon point is, the closest the pairs of prototypes are. The color 
used for filling the polygon shows the number of observations in each neuron. 
A white polygon means that there is no observation. With the default colors, a 
red polygon means a high number of observations;
\item \code{"umatrix"} (K, R) is another way of plotting distances between 
prototypes. The grid is plotted and filled with \code{my.palette} colors 
according to the mean distance between the current neuron and the neighboring 
neurons. With the default colors, red indicates proximity.
\item \code{"mds"} (K, R) plots the number of the neuron on a map according to 
a Multi Dimensional Scaling (MDS) projection on a two dimensional space.
\item \code{"grid.dist"} (K, R) plots on a 2 dimension map all distances. The 
number of points on this picture is equal to: 
\eqn{\frac{\textrm{number of neurons}\times(\textrm{number of neurons}-1)}{2}}{%
number of neurons  * (number of neurons - 1) / 2}. 
On the x axis corresponds to the prototype distances whereas the y axis depicts 
the grid distances.
}
}

\section{Graphics on an additional variable: \code{what="add"} (R)}{
The case \code{what="add"} considers an additional variable, which has to be 
given to the argument \code{variable}. Its length must match the number of
observations in the original data. Then the possible values for \code{type} are:
\code{"pie"}(R), \code{"color"}(R), \code{"lines"}(R), \code{"boxplot"}(R),
\code{"barplot"}(R), \code{"names"}(R), \code{"words"}(R) and
\code{"graph"}(R). 

\itemize{
\item \code{"color"} (R) has the same behavior as the \code{"color"} case 
described in the observations section. Then, the additional variable must be a 
numerical vector;
\item \code{"lines"} (R) has the same behavior as the \code{"lines"} case 
described in the observations section. Then, the additional variable must be a 
numerical matrix or a data frame;
\item \code{"boxplot"} (R) has the same behavior as the \code{"boxplot"} case 
described in the observations section. Then, the additional variable must be 
either a numeric vector or a numeric matrix/data frame;
\item \code{"barplot"} (R) has the same behavior as the \code{"barplot"} case 
described in the observations section. Then, the additional variable must be 
either a numeric vector or a numeric matrix/data frame;
\item \code{"pie"} requires the argument \code{variable} to be a vector, which
will be passed to the function \code{as.factor}, and plots one pie for each 
neuron according to this factor. By default, the size of the pie is 
proportional to the number of observations affected to its neuron but this can 
be changed with the argument \code{proportional = FALSE};
\item \code{"names"} (R) has the same behavior as the \code{"names"} case 
described in the observations section. Then, the names to be printed are the 
elements of the variable given to the \code{variable} argument;
\item \code{"words"} (R) needs the argument \code{variable} be a numeric matrix 
or a \code{data.frame}: names of the columns will be used as words and the 
values express the frequency of a given word in the observation. Then, for each 
neuron of the grid, the words will be printed with sizes proportional to the sum
of their values in the neuron. If the \code{variable} given is a contingency 
table, it will plot directly the frequency of the words in the neurons;
\item \code{"graph"} (R) requires that the argument \code{variable} is an
\code{igraph} object (see \code{library("igraph")}. According to the existing
edges in the graph and to the clustering obtained with the SOM algorithm, a 
clustered graph will be produced where a vertex between two vertices represents
a neuron and the width of an edge is proportional to the number of edges in the
given graph between the vertices affected to the corresponding neurons. The
option can handle two more arguments: \code{pie.graph} and \code{pie.variable}.
These are used to display the vertex as pie charts. For this case, 
\code{pie.graph} must be set to \code{TRUE} and a factor vector is supplied by
\code{pie.variable}.}

When the algorithm's type is \code{korresp}, no graphic is available for
\code{what = "add"}.

All these graphics are available for a \code{relational} SOM.}

\section{Further arguments via \dots}{
Further arguments, their reference functions and the \code{plot.somRes} cases 
are summarized in the following list:

\itemize{
\item \code{\link[igraph]{plot.igraph}} is called by the cases:
\itemize{
\item \code{what = "add"} / \code{type = "graph"}
\item \code{what = "add"} / \code{type = "projgraph"} (for a superclass object)
}

\item \code{\link[graphics]{persp}} is called by the case 
\code{what = "prototypes"} / \code{type = "3d"}

\item \code{\link[ggplot2]{ggplot}} is called in all the other cases. 
}

In complement to ggplot, 
\itemize{
\item \code{\link[ggwordcloud]{geom_text_wordcloud}} is called by the cases:
  \itemize{
  \item \code{type = "names"}
  \item \code{what = "add"} / \code{type = "words"}
  }

\item \code{\link[metR]{geom_contour_fill}} is called by the case
\code{what = "prototypes"} / \code{type = "smooth.dist"}
}
}

\author{Élise Maigné \email{elise.maigne@inrae.fr}\cr
Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
Nathalie Vialaneix \email{nathalie.vialaneix@inra.fr}
}

\examples{

### Numerical SOM
# run the SOM algorithm on the numerical data of 'iris' data set
iris.som <- trainSOM(x.data = iris[,1:4], nb.save = 2)

####### energy plot
plot(iris.som, what = "energy") # energy

####### plots on observations
plot(iris.som, what = "obs", type = "hitmap")
\dontrun{
plot(iris.som, what = "obs", type = "lines")
plot(iris.som, what = "obs", type = "barplot")
plot(iris.som, what = "obs", type = "boxplot")
plot(iris.som, what = "obs", type = "meanline")
plot(iris.som, what = "obs", type = "color", variable = 1)
plot(iris.som, what = "obs", type = "names")}

####### plots on prototypes
plot(iris.som, what = "prototypes", type = "3d", variable = "Sepal.Length")
\dontrun{
plot(iris.som, what = "prototypes", type = "lines")
plot(iris.som, what = "prototypes", type = "barplot)
plot(iris.som, what = "prototypes", type = "umatrix")
plot(iris.som, what = "prototypes", type = "color", variable = "Petal.Length")
plot(iris.som, what = "prototypes", type = "smooth.dist")
plot(iris.som, what = "prototypes", type = "poly.dist")
plot(iris.som, what = "prototypes", type = "grid.dist")
plot(iris.som, what = "prototypes", type = "mds")}


####### plots on an additional variable: the flower species
plot(iris.som, what = "add", type = "pie", variable = iris$Species)
\dontrun{
plot(iris.som, what = "add", type = "names", variable = iris$Species)
plot(iris.som, what = "add", type = "words", variable = iris[,1:2])}
}
