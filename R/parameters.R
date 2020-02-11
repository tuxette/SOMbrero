## These functions handle the parameters of SOM learning
## ----------------------------------------------------------------------------
#' @title Initialize parameters for the SOM algorithm
#' @export
#' @name initSOM
#' @exportClass paramSOM
#' 
#' @description 
#' The \code{initSOM} function returns a \code{paramSOM} class object that
#' contains the parameters needed to run the SOM algorithm.
#' 
#' @aliases summary.paramSOM
#' @aliases print.paramSOM
#' @aliases paramSOM-class
#' 
#' @param dimension Vector of two integer points corresponding to the x 
#' dimension and the y dimension of the \code{myGrid} class object. Default 
#' values are: \code{(5,5)}. Other data-driven defaults are set by function 
#' \code{trainSOM}.
#' @param topo The topology to be used to build the grid of the \code{myGrid} 
#' class object. Default value is \code{square}.
#' @param radius.type The neighbourhood type. Default value is 
#' \code{"gaussian"}, which corresponds to a Gaussian neighbourhood. The 
#' annealing of the neighbourhood during the training step is similar to the one
#' implemented in \href{https://github.com/fabrice-rossi/yasomi}{yasomi}. The 
#' alternative value corresponds to an piecewise linear neighbourhood as 
#' implementated by Patrick Letremy in his SAS scripts.
#' @param dist.type The neighborhood relationship on the grid. When 
#' \code{radius.type} is \code{letremy}, default value is \code{letremy} which 
#' is the original implementation by Patrick Letremy. When \code{radius.type} is
#' \code{gaussian}, default value is \code{euclidean}. The other possible values
#' (\code{maximum}, \code{manhattan}, \code{canberra}, \code{minkowski}) are 
#' passed to \code{method} in function \code{\link[stats]{dist}}. 
#' \code{dist.type="letremy"} is not permitted with 
#' \code{radius.type="gaussian"}.
#' @param type The SOM algorithm type. Possible values are: \code{numeric} 
#' (default value), \code{korresp} and \code{relational}.
#' @param mode The SOM algorithm mode. Default value is \code{online}.
#' @param affectation The SOM affectation type. Default value is \code{standard}
#' which corresponds to a hard affectation. Alternative is \code{heskes} which 
#' corresponds to Heskes's soft affectation.
#' @param maxit The maximum number of iterations to be done during the SOM 
#' algorithm process. Default value is \code{500}. Other data-driven defaults
#' are set by function \code{trainSOM}.
#' @param nb.save The number of intermediate back-ups to be done during the 
#' algorithm process. Default value is \code{0}.
#' @param verbose The boolean value which activates the verbose mode during the
#' SOM algorithm process. Default value is \code{FALSE}.
#' @param proto0 The initial prototypes. Default value is \code{NULL}.
#' @param init.proto The method to be used to initialize the prototypes, which
#' may be \code{random} (randomization), \code{obs} (each prototype is assigned
#' a random observation) or \code{pca}. In \code{pca} the prototypes are 
#' initialized to the observations closest to a grid along the two first 
#' principal components of the data (\code{numeric} case) or along a
#' two-dimensional multidimensional scaling (\code{relational} case, equivalent
#' to a \code{relational} PCA). Default value is \code{random} for the
#' \code{numeric} and \code{korresp} types, and \code{obs} for the  
#' \code{relational} type. \code{pca} is not available for \code{korresp} SOM.
#' @param scaling The type of data pre-processing. For \code{numeric} SOM, 
#' possibilities are \code{unitvar} (data are centered and scaled; this 
#' is the default value for a \code{numeric} SOM), \code{none} (no 
#' pre-processing), and \code{center} (data are centered but not scaled). For 
#' \code{korresp} SOM, the only available value is \code{chi2}. For 
#' \code{relational} SOM, possibilities are \code{none} (no pre-processing, 
#' default value for \code{relational} SOM) and \code{cosine}. This last one 
#' first turns the dissimilarity into a similarity using the suggestion in (Lee 
#' and Verleysen, 2007). Then, a cosine normalization as described in (Ben-Hur 
#' and Weston, 2010) is applied to the kernel, that is finally turned back into 
#' its induced distance. For further details on this processing, have a look at
#' the corresponding documentation in the directory "doc" of the package's 
#' installation directory.
#' @param eps0 The scaling value for the stochastic gradient descent step in the
#' prototypes' update. The scaling value for the stochastic gradient descent 
#' step is equal to 
#' \eqn{\frac{0.3\epsilon_0}{1+0.2t/\textrm{dim}}}{0.3*eps0/(1+0.2*t/dim)} where
#' \eqn{t}{t} is the current step number and \eqn{\textrm{dim}}{dim} is the grid
#' dimension (width multiplied by height).
#' @param x an object of class \code{paramSOM}.
#' @param object an object of class \code{paramSOM}.
#' @param \dots not used
#' 
#' @return The \code{initSOM} function returns an object of class 
#' \code{paramSOM} which is a list of the parameters passed to the 
#' \code{initSOM} function, plus the default parameters for the ones not 
#' specified by the user.
#' 
#' @author Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @references
#' Ben-Hur A., Weston J. (2010) A user's guide to support vector machine. In:
#' \emph{Data Mining Techniques for the Life Sciences}, Springer-Verlag, 
#' 223-239.
#' 
#' Heskes T. (1999) Energy functions for self-organizing maps. In: \emph{Kohonen
#' Maps}, Oja E., Kaski S. (Eds.), Elsevier, 303-315.
#' 
#' Lee J., Verleysen M. (2007) \emph{Nonlinear Dimensionality Reduction}.
#' Information Science and Statistics series, Springer.
#' 
#' Letrémy P. (2005) Programmes basés sur l'algorithme de Kohonen et dediés à
#' l'analyse des données. SAS/IML programs for 'korresp'.
#' \url{http://samm.univ-paris1.fr/Programmes-SAS-de-cartes-auto}.
#' 
#' Rossi F. (2013) yasomi: Yet Another Self-Organising Map Implementation. R 
#' package, version 0.3. \url{https://github.com/fabrice-rossi/yasomi}
#' 
#' @seealso See \code{\link{initGrid}} for creating a SOM prior structure 
#' (grid).
#' 
#' @examples 
#' # create a default 'paramSOM' class object
#' default.paramSOM <- initSOM()
#' summary(default.paramSOM)

initSOM <- function(dimension=c(5,5), topo=c("square"),
                    radius.type=c("gaussian", "letremy"),
                    dist.type=switch(match.arg(radius.type), 
                                     "letremy"="letremy", 
                                     "gaussian"="euclidean"),
                    type=c("numeric", "relational", "korresp"), 
                    mode=c("online"), affectation=c("standard", "heskes"),
                    maxit=500, nb.save=0, verbose=FALSE, proto0=NULL, 
                    init.proto=switch(type,
                                      "numeric"="random",
                                      "relational"="obs",
                                      "korresp"="random"),
                    scaling=switch(type,
                                   "numeric"="unitvar",
                                   "relational"="none",
                                   "korresp"="chi2"), eps0=1) {
  type <- match.arg(type)
  radius.type <- match.arg(radius.type)
  affectation <- match.arg(affectation)
  scaling <- match.arg(scaling,
                       c("unitvar", "none", "center", "chi2", "cosine"))
  dist.type <- match.arg(dist.type, c("letremy", "maximum", "euclidean",
                                      "manhattan", "canberra", "minkowski"))
  
  if (dist.type=="letremy" && radius.type=="gaussian") {
    dist.type <- "euclidean"
    warning("dist.type value replaced to 'euclidean' for Gaussian radius\n
            ('letremy' is not allowed)\n", 
            call.=TRUE, immediate.=TRUE)
  }
  
  init.proto <- match.arg(init.proto, c("random", "obs", "pca"))
  
  # check scaling compatibility
  if (type=="korresp" && scaling!="chi2") {
    scaling <- "chi2"
    warning("scaling value replaced: must be 'chi2' for 'korresp' type\n", 
            call.=TRUE, immediate.=TRUE)
  }
  if (type=="relational" && ! scaling %in% c("none", "cosine")) {
    scaling <- "none"
    warning("Wrong scaling for 'relational' SOM ; set to 'none'\n", call.=TRUE, 
            immediate.=TRUE)
  }
  if (type=="numeric" && scaling %in% c("chi2", "cosine"))
    stop(paste0("scaling='", scaling,
                "' is only implemented for 'korresp' type\n"), call.=TRUE)
  
  # check init.proto compatibility
  if (type=="korresp" && init.proto=="pca")
    stop("'init.proto' cannot be 'pca' for 'korresp' type\n", call.= TRUE)
  
  # check proto0
  if (!is.null(proto0)) {
    if (type=="relational") {
      if (sum(proto0<0)>0)
        stop("initial prototypes given by user do not match chosen type.
             Prototype values must be greater than 0.\n", call.=TRUE)
      if (sum(rowSums(proto0)!=1)>0)
        stop("initial prototypes given by user do not match chosen type.
             Prototype row sums for 'relational' must be equal to 1\n", 
             call.=TRUE)
    }
    if (type=="korresp") {
      if (min(proto0)<0 || max(proto0)>1)
        stop("initial prototypes given by user do not match chosen type.
               Prototypes for 'korresp' must have values between 0 and 1\n", 
             call.=TRUE)
    }
  }
  
  params <- list("the.grid"=initGrid(dimension,match.arg(topo),
                                     dist.type),
                 type=type, mode=match.arg(mode), affectation=affectation,
                 maxit=maxit, nb.save=nb.save, proto0=proto0,
                 init.proto=init.proto, scaling=scaling, 
                 radius.type=radius.type, verbose=verbose, eps0=eps0)
  
  class(params) <- "paramSOM"
  
  return(params)
}

#' @export
#' @rdname initSOM
print.paramSOM <- function(x,...){
  cat("\n  Parameters of the SOM\n\n")
  cat("    SOM mode                       : ", x$mode, "\n")
  cat("    SOM type                       : ", x$type, "\n")
  cat("    Affectation type               : ", x$affectation, "\n")
  cat("    Grid                           : ")
  print(x$the.grid)
  cat("    Number of iterations           : ", x$maxit, "\n")
  cat("    Number of intermediate backups : ", x$nb.save, "\n")
  if(identical(x$proto0,NULL)){
    cat("    Initializing prototypes method : ", x$init.proto, "\n")
  } else {
    cat("    Initial prototypes given by user\n")
  }
  cat("    Data pre-processing type       : ", x$scaling, "\n")
  cat("    Neighbourhood type             : ", x$radius.type, "\n")
  cat("\n")
}

#' @export
#' @rdname initSOM
summary.paramSOM <- function(object,...){
  cat("\nSummary\n\n")
  cat("  Class                            : ", class(object),"\n")
  print(object)
}