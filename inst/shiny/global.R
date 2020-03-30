####################################################################################
# Libraries
####################################################################################
library(shinycssloaders) # for the loader of the summary
library(shinyBS)  # for bsCollapse
library(shinyjs) # Version hide/show/toggle
library(shinyjqui)
library(gridExtra)
library(SOMbrero) # Version 0.4

jscode <- "
shinyjs.refocus = function() {
  window.scrollTo(0,0);
}"

####################################################################################
# Loading the data from environment and examples
####################################################################################
# data.frames in the global environment
data("iris")
data("lesmis")
data("presidentielles2002")

# tibbles and data.tables are also returned since they are in the data.frame class
dataframes <- ls()[sapply(ls(envir = .GlobalEnv), function(x) 'data.frame' %in% class(get(x)) | 'matrix' %in% class(get(x)))]

###############################################################################
## Global variables
####################################################################################

# Max file input size :
options(shiny.maxRequestSize=30*1024^2)

# SOM training function
trainTheSom <- function(data, type, topo, dimx, dimy, affectation, disttype, maxit,
                        varnames, rand.seed, scaling, eps0, init.proto, nb.save,
                        radiustype) {
  #if (type=="numeric")
  data <- data[, varnames]
  set.seed(rand.seed)
  trainSOM(data, topo=topo, dimension=c(dimx,dimy), affectation=affectation, 
           dist.type=disttype, maxit=maxit, type=type, scaling=scaling, eps0=eps0, 
           init.proto=init.proto, nb.save=nb.save, radius.type=radiustype)
}

# List of somplot types options per SOM type and "what" :
all.somplot.types <- list("numeric"=
                            list("prototypes"=
                                   list("lines", "barplot", "color", "3d", 
                                        "smooth distances"="smooth.dist",
                                        "polygon distances"="poly.dist",
                                        "grid distances"="grid.dist",
                                        "U matrix distances"="umatrix",
                                        "MDS"="mds"),
                                 "obs"=c("hitmap", "color", "lines", "barplot", 
                                         "names", "boxplot"),
                                 "energy"="Energy of backups"),
                          
                          "korresp"=
                            list("prototypes"=
                                   list("lines", "barplot", "color", "3d", 
                                        "polygon distances"="poly.dist",
                                        "grid distances"="grid.dist",
                                        "U matrix distances"="umatrix",
                                        "MDS"="mds"),
                                 "obs"=c("hitmap", "names"),
                                 "energy"="Energy of backups"),
                          
                          "relational"=
                            list("prototypes"=
                                   list("lines", "barplot",
                                        "polygon distances"="poly.dist",
                                        "grid distances"="grid.dist",
                                        "U matrix distances"="umatrix",
                                        "MDS"="mds"),
                                 "obs"=c("hitmap", "names"),
                                 "energy"="Energy of backups"))

all.scplot.types <- list("numeric"=
                           list("prototypes"=
                                  list("grid", "dendrogram", "dendro3d", "color",
                                       "lines", "barplot", 
                                       "polygon distances"="poly.dist",
                                       "MDS"="mds"),
                                "obs"=c("hitmap", "color", "lines", "barplot", 
                                        "boxplot", "grid")),
                         "korresp"=
                           list("prototypes"=
                                  list("grid", "dendrogram", "color", "lines", 
                                       "barplot", 
                                       "polygon distances"="poly.dist",
                                       "MDS"="mds", "dendro3d"),
                                "obs"="hitmap"),
                         "relational"=
                           list("prototypes"=
                                  list("grid", "dendrogram", "lines", "barplot", 
                                       "polygon distances"="poly.dist",
                                       "MDS"="mds",  "dendro3d"),
                                "obs"="hitmap"))

