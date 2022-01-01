# Helpers for missing data ####################################################

#' @title Impute values from prototype information
#' @export
#' @name impute
#' @aliases impute.somRes
#' 
#' @description Impute values by replacing missing entries with the 
#' corresponding assigned prototype entries
#' 
#' @param object a \code{somRes} object.
#' 
#' @return Imputed matrix as in [Cottrell and Letremy, 2005]
#' 
#' @author 
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @references 
#' Cottrell M., Letremy P. (2005) Missing values: processing with the Kohonen 
#' algorithm. \emph{Proceedings of Applied Stochastic Models and Data Analysis
#' (ASMDA 2005)}, 489-496.
#' 
#' @seealso \code{\link{trainSOM}}
#' 
#' @examples
#' # Run trainSOM algorithm on the iris data with 500 iterations
#' set.seed(1505)
#' missings <- cbind(sample(1:150, 50, replace = TRUE),
#'                   sample(1:4, 50, replace = TRUE))
#' x.data <- as.matrix(iris[, 1:4])
#' x.data[missings] <- NA
#' iris.som <- trainSOM(x.data = x.data)
#' iris.som
#' impute(iris.som)

impute <- function(object, ...) {
  UseMethod("impute")
}

#' @export
impute.somRes <- function(object, ...) {
  imputations <- imputeSOM(object$data, object$clustering, object$prototypes)
  return(imputations)
}

## Impute by mean
mean.imputation <- function(x.data) {
  data.miss <- apply(x.data, 2, function(acol) sum(is.na(acol)))
  
  if (any(data.miss) > 0) {
    data.pmiss <- x.data[, data.miss > 0, drop = FALSE]
    data.pmiss <- apply(data.pmiss, 2, function(acol) {
      acol[is.na(acol)] <- mean(acol, na.rm = TRUE)
      return(acol)
    })
    x.data[, data.miss > 0] <- data.pmiss
  }
  
  return(x.data)
}

## Impute with SOM results
imputeSOM <- function(x.data, clustering, prototypes) {
  imputed.data <- sapply(1:nrow(x.data), function(arow) 
    imputeOneObs(x.data[arow, ], clustering[arow], prototypes = prototypes))
  return(t(imputed.data))
}

imputeOneObs <- function(obs, cluster, prototypes) {
  if (any(is.na(obs))) {
    selected <- is.na(obs)
    obs[selected] <- prototypes[cluster, selected]
  }
  return(obs)
}