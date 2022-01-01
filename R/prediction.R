# SOM prediction functions ####

#' @title Predict the classification of a new observation
#' @export
#' @name predict.somRes
#' 
#' @description Predict the neuron where a new observation is classified
#' 
#' @param object a \code{somRes} object.
#' @param x.new a new observation (optional). Default values is NULL which
#' corresponds to performing prediction on the training dataset.
#' @param \dots not used.
#' @param radius current radius used to perform soft affectation (when 
#' \code{affectation = "heskes"}, see \code{\link{initSOM}} for further details
#' about Heskes' soft affectation). Default value is \option{0}, 
#' which corresponds to a hard affectation.
#' 
#' @details The number of columns of the new observations (or its length if only 
#' one observation is provided) must match the number of columns of the data set
#' given to the SOM algorithm (see \code{\link{trainSOM}}).
#' 
#' @return \code{predict.somRes} returns the number of the neuron to which the 
#' new observation is assigned (i.e., neuron with the closest prototype).
#' 
#' When the algorithm's type is \code{"korresp"}, \code{x.new} must be the 
#' original contingency table passed to the algorithm.
#' 
#' @author Jérome Mariette \email{jerome.mariette@inrae.fr}\cr
#' Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Fabrice Rossi \email{fabrice.rossi@apiacoa.org}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{trainSOM}}
#' 
#' @examples 
#' set.seed(2343)
#' my.som <- trainSOM(x.data = iris[-100, 1:4], dimension = c(5, 5))
#' predict(my.som, iris[100, 1:4])

predict.somRes <- function(object, x.new = NULL, ..., radius = 0) {
  winners <- switch(object$parameters$type,
                    "numeric"    = predictionNumericSOM(object, x.new, radius),
                    "relational" = predictionRelationalSOM(object, x.new, radius),
                    "korresp"    = predictionKorrespSOM(object, x.new, radius))

  return(winners)  
}


## Prediction for numeric SOM
predictionNumericSOM <- function(object, x.new, radius) {
  if (is.null(x.new)) {
    x.new <- object$data
  } else { # preprocess
    if (is.null(dim(x.new)))
      x.new <- matrix(x.new, nrow = 1, dimnames = list(1, colnames(object$data)))
    if (!is.matrix(x.new)) x.new <- as.matrix(x.new)
    # check data dimension
    if (ncol(x.new) != ncol(object$data))
      stop("Number of columns of x.new does not correspond to number of columns of the original data")
  }
  
  norm.x.new <- preprocessProto(x.new, object$parameters$scaling, object$data)
  norm.proto <- preprocessProto(object$prototypes, object$parameters$scaling,
                                object$data)
  
  winners <- obsAffectation(norm.x.new, prototypes = norm.proto,
                            type = object$parameters$type,
                            affectation = object$parameters$affectation,
                            radius.type = object$parameters$radius.type,
                            radius = radius, 
                            the.grid = object$parameters$the.grid)
  
  if (is.null(names(winners))) names(winners) <- rownames(x.new)
  return(winners)
}


## Prediction for numeric SOM
predictionRelationalSOM <- function(object, x.new, radius) {
  if (object$parameters$scaling == "cosine") 
    stop("'predict' is not implemented for cosine datasets")
  
  if (is.null(x.new)) {
    x.new <- object$data
  } else {
    if (is.null(dim(x.new)))
      x.new <- matrix(x.new, nrow = 1, dimnames = list(1, colnames(object$data)))
    
    if (!is.matrix(x.new)) x.new <- as.matrix(x.new)
    
    # check data dimension
    if (ncol(x.new) != ncol(object$data))
      stop("Number of columns of x.new does not correspond to number of columns of the original data")
    
  }
  
  winners <- obsAffectation(x.new, prototypes = object$prototypes,
                            type = object$parameters$type, 
                            x.data = object$data,
                            affectation = object$parameters$affectation,
                            radius.type = object$parameters$radius.type,
                            radius = radius, 
                            the.grid = object$parameters$the.grid)
  
  return(winners)
}


## Prediction for numeric SOM
predictionKorrespSOM <- function(object, x.new, radius) {
  if (!is.null(x.new)) 
    warning("For 'korresp' SOM, predict.somRes function can only be called on
              the original data set\n'object'. x.new not used!", 
            call. = TRUE)
  
  norm.x.data <- korrespPreprocess(object$data)
  
  data.rows <- norm.x.data[1:nrow(object$data), 1:ncol(object$data)]
  data.cols <- norm.x.data[(nrow(object$data)+1):ncol(norm.x.data),
                           (ncol(object$data)+1):ncol(norm.x.data)]
  proto.rows <- object$prototypes[, 1:ncol(object$data)]
  proto.cols <- object$prototypes[, (ncol(object$data) + 1):ncol(norm.x.data)]
  
  winners.rows <- obsAffectation(data.rows, proto.rows,
                                 type = object$parameters$type,
                                 affectation = object$parameters$affectation,
                                 radius.type = object$parameters$radius.type, 
                                 radius = radius, 
                                 the.grid = object$parameters$the.grid)
  
  winners.cols <- obsAffectation(data.cols, proto.cols,
                                 type = object$parameters$type,
                                 affectation = object$parameters$affectation,
                                 radius.type = object$parameters$radius.type, 
                                 radius = radius, 
                                 the.grid = object$parameters$the.grid)
    
  
  winners <- c(winners.cols, winners.rows)
  return(winners)
}


## Fast prediction for relational SOM

predictRSOM <- function(object, x.new = NULL, radius = 0, A = NULL, B = NULL) {
  if (is.null(A) || is.null(B)) {
    winners <- predict.somRes(object, x.new = x.new, radius = radius)
  } else {
    # distance between all prototypes and all data
    all.dist <- sweep(t(B), 1, -0.5 * A, "+")
    # affectation to the closest prototype
    if (object$parameters$affectation == "standard") {
      winners <- apply(all.dist, 2, which.min)
    } else {
      # Heskes' soft affectation
      u.weights <- sapply(1:nrow(object$prototypes), function(a.neuron) {
        the.nei <- selectNei(a.neuron, object$parameters$the.grid, radius, 
                             object$parameters$radius.type,
                             object$parameters$the.grid$dist.type)
        return(the.nei)
      })
      if (object$parameters$radius.type != "letremy") {
        w.dist <- t(apply(u.weights, 1, function(awproto) {
          apply(sweep(all.dist, 1, awproto, "*"), 2, sum)
        }))
      } else {
        if (radius == 0) {
          w.dist <- all.dist
        } else {
          w.dist <- lapply(u.weights, function(awproto) {
            apply(all.dist[awproto, ], 2, sum)
          })
          w.dist <- matrix(unlist(w.dist), nrow = nrow(object$prototypes), 
                           byrow = TRUE)
        }
      }
      winners <- apply(w.dist, 2, which.min)
    }
  }
  return(winners)
}