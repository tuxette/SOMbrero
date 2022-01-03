# SOM algorithm functions ####

## Auxiliary functions ####
calculateRadius <- function(the.grid, radius.type, ind.t, maxit) {
  # ind.t: iteration index
  if (radius.type == "letremy") {
    r0 <- max(floor(the.grid$dim / 2))
    k <- 4 * (r0-1) / maxit
    a <- floor(maxit / 2)
    b <- floor(maxit * 3/4)
    r <- ceiling(r0 / (1 + k * ind.t))
    if (ind.t==1) {
      r <- r0
    } else if (ind.t >= a & ind.t < b) {
      r <- 0.5
    } else if (ind.t >= b) {
      r <- 0
    }
  } else if (radius.type == "gaussian") {
      r0 <- 1 + (2/3) * sqrt(sum(the.grid$dim^2))
      r <- r0 * (1/r0)^((ind.t - 1) / (maxit - 1)) - 1 # power decrease 
      r <- max(0, r)
  }
  
  return(r)
}

selectNei <- function(the.neuron, the.grid, radius, radius.type, 
                      dist.type = the.grid$dist.type) {
  tolerance <- 10^(-13) # numerical tolerance to improve stability (hard coded)
  
  if (radius.type == "letremy") { # output is a list of neurons
    if (dist.type == "letremy") {
      if (radius == 0.5) {
        the.dist <- dist(the.grid$coord, diag = TRUE, upper = TRUE, 
                         method = "euclidean")
        the.dist <- as.matrix(the.dist)[the.neuron, ]
        the.nei <- which(the.dist <= 1)
      } else {
        the.dist <- dist(the.grid$coord, diag = TRUE, upper = TRUE,
                         method = "maximum")
        the.dist <- as.matrix(the.dist)[the.neuron, ]
        the.nei <- which(the.dist <= radius)
      }
    } else { # one of the distance implemented in dist
      dist.grid <- dist(the.grid$coord, diag = TRUE, upper = TRUE,
                        method = dist.type)
      the.dist <- as.matrix(dist.grid)[the.neuron, ]
      the.nei <- which(the.dist - radius <= tolerance)
    }
  } else if (radius.type == "gaussian") { 
    # output is a numeric value of proximity with all neurons
    proto.dist <- dist(the.grid$coord, upper = TRUE, diag = TRUE, 
                       method = dist.type)
    proto.dist <- as.matrix(proto.dist)
    sigma <- quantile(proto.dist[upper.tri(proto.dist)]^2, 0.5)
    the.nei <- exp(- proto.dist[the.neuron, ]^2 / sigma / (radius + 1)^2)
  }
  
  return(the.nei)
}

# Functions to manipulate objects in the input space
distEuclidean <- function(x, y) {
  sqrt(sum((x - y)^2))
}

distRelationalProto <- function(proto1, proto2, x.data) {
  -0.5 * t(proto1 - proto2) %*% x.data %*% (proto1 - proto2)
}

## Functions used during training of SOM ####
### Step 5: Randomly choose an observation ####
selectObs <- function(ind.t, ddim, type) {
  if (type == "korresp") {
    rand.ind <- ifelse(ind.t%%2 == 0,
                       sample(1:ddim[1], 1), # iteration is even: sample rows
                       sample((ddim[1] + 1):(ddim[1] + ddim[2]), 1)) # otherwise
  } else rand.ind <- sample(1:ddim[1], 1)
  
  return(rand.ind) 
}

### Step 6: Assignment step ####
# assign one observation only
oneObsAffectation <- function(x.new, prototypes, type, affectation, 
                              x.data = NULL, radius.type = NULL, radius = NULL,
                              the.grid = NULL, the.dist = NULL) {
  
  if (type == "relational" & is.null(the.dist)) {
    
    all.dist1 <- prototypes %*% x.new
    all.dist2 <- - 0.5 * diag(prototypes %*% tcrossprod(x.data, prototypes))
    the.dist <- all.dist1 + all.dist2
    
  } else { # type is 'numeric' or 'korresp' - NAs are handled for 'numeric'
    
    not.missing <- !is.na(x.new)
    prototypes <- prototypes[, not.missing]
    x.new <- x.new[not.missing]
    
    the.dist <- sweep(prototypes, 2, x.new, "-")
    the.dist <- rowSums(the.dist^2)
    
  }
  
  if (affectation != "standard") {
    final.dist <- sapply(1:nrow(prototypes), selectNei,
                         the.grid = the.grid, radius = radius, 
                         radius.type = radius.type, 
                         dist.type = the.grid$dist.type)
      
    if (radius.type != "letremy") {
      
      # sum of distances weighted by final.dist
      weight.nei <- rowSums(final.dist)
      final.dist <- sweep(final.dist, 2, the.dist, "*")
      final.dist <- rowSums(final.dist) / weight.nei
      
    } else { # 'radius.type' is "letremy"
      
      # average of distances selected in final.dist
      nb.nei <- sapply(final.dist, length)
      final.dist <- sapply(final.dist, function(alist) the.dist[alist])
      # note: divide by nb.nei in order to not favor border of the grid
      final.dist <- sapply(final.dist, sum) / nb.nei
      
    }
      
    the.dist <- final.dist
  }
  
  return(which.min(the.dist))
}

obsAffectation <- function(x.new, prototypes, type, affectation, x.data = NULL,
                           radius.type = NULL, radius = NULL, the.grid = NULL) {
  if (is.null(dim(x.new)) || nrow(x.new) == 1) { 
    # one element -> back to oneObsAffectation
    if (!is.null(dim(x.new))) x.new <- x.new[1, ]
    the.neuron <- oneObsAffectation(x.new, prototypes, type, affectation, 
                                    x.data, radius.type, radius, the.grid)
    return(the.neuron)
  } 

  # distance between all prototypes and all data
  # kept in case new data is added in the model
  if (type == "relational") {
    dist.1 <- - 0.5 * diag(prototypes %*% x.data %*% t(prototypes))
    dist.2 <- tcrossprod(prototypes, x.new)
    all.dist <- sweep(dist.2, 1, dist.1, "+")
  } else {
    if (sum(is.na(x.new)) == 0) {
      # Euclidean distance
      dist.1 <- - 2 * tcrossprod(prototypes, x.new)
      dist.2 <- diag(tcrossprod(prototypes, prototypes))
      all.dist <- sweep(dist.1, 1, dist.2, "+")
    } else {
      all.dist <- sapply(1:nrow(x.new), function(irow) {
        not.missing <- !is.na(x.new[irow, ])
        prototypes <- prototypes[, not.missing, drop = FALSE]
        arow <- x.new[irow, not.missing, drop = FALSE]
        dist.1 <- -2 * tcrossprod(prototypes, arow)
        dist.2 <- diag(tcrossprod(prototypes, prototypes))
        out <- sweep(dist.1, 1, dist.2, "+")
        return(out)
      })
    }
  }
  
  # affectation to the closest prototype
  if (affectation == "standard") return(apply(all.dist, 2, which.min))
  
  # OR: Heskes' soft affectation
  u.weights <- sapply(1:nrow(prototypes), selectNei,
                      the.grid = the.grid, radius = radius, 
                      radius.type = radius.type, 
                      dist.type = the.grid$dist.type)
  
  if (type != "relational")
    all.dist <- sweep(all.dist, 2, apply(x.new^2, 1, sum, na.rm = TRUE), "+")
  
  if (radius.type != "letremy") {
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
      w.dist <- matrix(unlist(w.dist), nrow = nrow(prototypes), byrow = TRUE)
    }
  }
  
  out <- apply(w.dist, 2, which.min)
  return(unlist(out))
}

## Step 7: Update of prototypes ####
prototypeUpdate <- function(type, the.nei, epsilon, prototypes, sel.obs, 
                            radius.type, winner) {
  if (type != "relational") # handling missing data for 'numeric' by removing them
    to.update <- !is.na(sel.obs)
  
  nb.proto <- nrow(prototypes)
  
  if (radius.type != "letremy") {
    
    if (type == "relational") {
      indic <- matrix(0, nrow = nb.proto, ncol = ncol(prototypes)) 
      indic[, sel.obs] <- 1
      prototypes <- (1 - epsilon * the.nei) * prototypes
      prototypes <- prototypes + epsilon * the.nei * indic
    } else { # 'numeric' or 'korresp'
      # NAs are handled in 'numeric' by not using the corresponding entries
      prototypes[, to.update] <- 
        (1 - epsilon * the.nei) * prototypes[, to.update]
      update2 <- epsilon * the.nei * outer(rep(1, nb.proto), na.omit(sel.obs))
      prototypes[, to.update] <- prototypes[, to.update] + update2
    }
    
  } else { # radius.type is 'gaussian'
    
    if (type == "relational") {
      indic <- matrix(0, nrow = length(the.nei), ncol = ncol(prototypes)) 
      indic[, sel.obs] <- 1
      prototypes[the.nei, ] <- (1 - epsilon) * prototypes[the.nei, ] 
      prototypes[the.nei, ] <- prototypes[the.nei, ] + epsilon * indic    
    } else {
      prototypes[the.nei, to.update] <- 
        (1 - epsilon) * prototypes[the.nei, to.update] 
      update2 <- epsilon * outer(rep(1, length(the.nei)), na.omit(sel.obs))
      prototypes[the.nei, to.update] <- prototypes[the.nei, to.update] + update2
    }
    
  }
  
  return(prototypes)
}

## Step 8: calculate intermediate energy ####
# TODO: It would probably be better to implement a function 'distEltProto' or to have a global direct version

calculateClusterEnergy <- function(cluster, x.data, clustering, prototypes,
                                   parameters, radius) {
  the.nei <- selectNei(cluster, parameters$the.grid, radius, 
                       parameters$radius.type, parameters$the.grid$dist.type)
  
  if (parameters$radius.type == "letremy")
    the.nei <- as.numeric((1:nrow(prototypes)) %in% the.nei)
  
  if (parameters$type == "numeric" || parameters$type == "korresp") {
    
    # energy is based on imputation of x with prototypes
    if (any(is.na(x.data))) {
      x.data <- imputeSOM(x.data, clustering, prototypes)
    }
    
    values <- x.data - outer(rep(1, nrow(x.data)), prototypes[cluster, ])
    values <- rowSums(values^2)
    return(sum(the.nei[clustering] * values))
  } else if (parameters$type == "relational") {
    
    values <- prototypes[cluster, , drop = FALSE] %*% x.data
    values2 <- 0.5 * tcrossprod(x.data, prototypes[cluster, , drop = FALSE])
    values2 <- prototypes[cluster, , drop = FALSE] %*% values2
    values <- values - diag(values2)
    return(sum(the.nei[clustering] *values))
  }
}

calculateEnergy <- function(x.data, clustering, prototypes, parameters, ind.t) {
  radius <- calculateRadius(parameters$the.grid, parameters$radius.type, ind.t, 
                            parameters$maxit)
  energy <- sapply(1:nrow(prototypes), calculateClusterEnergy, x.data = x.data,
                   clustering = clustering, prototypes = prototypes, 
                   parameters = parameters, radius = radius)
  energy <- sum(unlist(energy)) / nrow(x.data) / nrow(prototypes)
  return(energy)
}

## Main function ##############################################################

#' @title Run the SOM algorithm
#' @export
#' @name trainSOM
#' 
#' @description The \code{trainSOM} function returns a \code{somRes} class 
#' object which contains the outputs of the algorithm.
#' 
#' @aliases summary.somRes
#' @aliases print.somRes
#' 
#' @param x.data a data frame or matrix containing the observations to be mapped
#' on the grid by the SOM algorithm.
#' @param \dots Further arguments to be passed to the function 
#' \code{\link{initSOM}} for specifying the parameters of the algorithm. The 
#' default values of the arguments \code{maxit} and \code{dimension} are 
#' calculated according to the SOM type if the user does not set them:
#' \itemize{
#'   \item \code{maxit} is equal to (number of rows+number of columns)*5 if the 
#'   SOM type is \code{korresp}. It is equal to number of rows*5 in all other 
#'   SOM types
#'   \item \code{dimension}: for a \code{korresp} SOM, is approximately equal to
#'   the square root of the number of observations to be classified divided by
#'   10 but it is never smaller than 5 or larger than 10.
#' }
#' @param x an object of class \code{somRes}.
#' @param object an object of class \code{somRes}.
#' 
#' @return The \code{trainSOM} function returns an object of class \code{somRes}
#' which contains the following components:
#' \itemize{
#'   \item{clustering}{ the final classification of the data.}
#'   \item{prototypes}{ the final coordinates of the prototypes.}
#'   \item{energy}{ the final energy of the map. For the numeric case, energy 
#'   with data having missing entries is based on data imputation as described
#'   in Cottrell and Letrémy (2005b).}
#'   \item{backup}{ a list containing some intermediate backups of the 
#'   prototypes coordinates, clustering, energy and the indexes of the recorded 
#'   backups, if \code{nb.save} is set to a value larger than 1.}
#'   \item{data}{ the original dataset used to train the algorithm.}
#'   \item{parameters}{ a list of the map's parameters, which is an object of 
#'   class \code{paramSOM} as produced by the function \code{\link{initSOM}}.}
#' }
#' The function \code{summary.somRes} also provides an ANOVA (ANalysis Of 
#' VAriance) of each input numeric variables in function of the map's clusters. 
#' This is helpful to see which variables participate to the clustering.
#' 
#' @details The version of the SOM algorithm implemented in this package is the
#' stochastic version.
#' 
#' Several variants able to handle non-vectorial data are also implemented in 
#' their stochastic versions: \code{type="korresp"} for contingency tables, as
#' described in Cottrell et al. (2004) (with weights as in Cottrell and Letrémy, 
#' 2005a); \code{type = "relational"} for dissimilarity matrices, as described 
#' in Olteanu et al. (2015), with the fast implementation introduced in Mariette
#' \emph{et al.} (2017).
#' 
#' Missing values are handled as described in Cottrell et al. (2005b), not using
#' missing entries of the selected observation during winner computation or 
#' prototype updates. This allows to proceed with the imputation of missing
#' entries with the corresponding entries of the cluster prototype (with 
#' \code{\link{impute}}).
#' 
#' \code{\link{summary}} produces a complete summary of the results that 
#' displays the parameters of the SOM, quality criteria and ANOVA. For 
#' \code{type = "numeric"} the ANOVA is performed for each input variable and 
#' test the difference of this variable across the clusters of the map. For 
#' \code{type = "relational"} a dissimilarity ANOVA is performed (Anderson, 
#' 2001), except that in the present version, a crude estimate of the p-value is
#' used which is based on the Fisher distribution and not on a permutation test.
#' 
#' @references 
#' Anderson M.J. (2001). A new method for non-parametric multivariate analysis 
#' of variance. \emph{Austral Ecology}, \strong{26}, 32-46. 
#' 
#' Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg: 
#' Springer-Verlag, 3rd edition.
#' 
#' Cottrell M., Ibbou S., Letrémy P. (2004) SOM-based algorithms for qualitative
#' variables. \emph{Neural Networks}, \strong{17}, 1149-1167.
#' 
#' Cottrell M., Letrémy P. (2005a) How to use the Kohonen algorithm to 
#' simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
#' \strong{21}, 119-138.
#' 
#' Cottrell M., Letrémy P. (2005b) Missing values: processing with the Kohonen 
#' algorithm. \emph{Proceedings of Applied Stochastic Models and Data Analysis
#' (ASMDA 2005)}, 489-496.
#' 
#' Olteanu M., Villa-Vialaneix N. (2015) On-line relational and multiple
#' relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 
#' 
#' Mariette J., Rossi F., Olteanu M., Mariette J. (2017) Accelerating stochastic 
#' kernel SOM. In: M. Verleysen, \emph{XXVth European Symposium on Artificial 
#' Neural Networks, Computational Intelligence and Machine Learning (ESANN 
#' 2017)}, i6doc, Bruges, Belgium, 269-274.
#' 
#' @author Élise Maigné <elise.maigne@inrae.fr>\cr
#' Jérome Mariette \email{jerome.mariette@inrae.fr}\cr
#' Madalina Olteanu \email{olteanu@ceremade.dauphine.fr}\cr
#' Fabrice Rossi \email{fabrice.rossi@apiacoa.org}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#'
#' @note Warning! Recording intermediate backups with the argument 
#' \code{nb.save} can strongly increase the computational time since calculating
#' the entire clustering and the energy is time consuming. Use this option with
#' care and only when it is strictly necessary.
#' 
#' @seealso See \code{\link{initSOM}} for a description of the parameters to 
#' pass to the trainSOM function to change its behavior and 
#' \code{\link{plot.somRes}} to plot the outputs of the algorithm.
#' 
#' @examples 
#' # Run trainSOM algorithm on the iris data with 500 iterations
#' iris.som <- trainSOM(x.data=iris[,1:4])
#' iris.som
#' summary(iris.som)

trainSOM <- function(x.data, ...) {
  UseMethod("trainSOM")
}

#' @export
trainSOM.matrix <- function(x.data, ...) {
  res <- run.trainSOM(x.data, ...)
  return(res)
}

#' @export
trainSOM.data.frame <- function(x.data, ...) {
  x.data <- as.matrix(x.data)
  res <- run.trainSOM(x.data, ...)
  return(res)
}

#' @export
trainSOM.dist <- function (x.data, ...) {
  x.data <- as.matrix(x.data)
  res <- run.trainSOM(x.data, ...)
  return(res)
}

#' @export
trainSOM.kernel <- function (x.data, ...) {
  ## TODO: check that in mixKernel: does 'kernel' inherits from 'matrix'?
  kernel.diag <- diag(x.data$kernel)
  dissim <- sweep(-2 * x.data$kernel, 1, kernel.diag, "+")
  dissim <- sweep(dissim, 2, kernel.diag, "+")
  dissim <- (t(dissim) + dissim) / 2
  
  res <- run.trainSOM(dissim, ...)
  return(res)
}

run.trainSOM <- function (x.data, ...) {
  param.args <- list(...)
  ## Step 1: Parameters handling
  if (!is.matrix(x.data)) x.data <- as.matrix(x.data)
  if (is.null(rownames(x.data))) rownames(x.data) <- 1:nrow(x.data)
  if (is.null(colnames(x.data))) colnames(x.data) <- paste0("X", 1:ncol(x.data))
  
  # Check inputs
  notnum <- apply(x.data, 2, class) 
  notnum <- notnum[!(notnum %in% c("integer", "numeric"))]
  if (!is.null(param.args$type) && param.args$type == "korresp" &&
      length(notnum) > 0)
    stop("data do not match chosen SOM type ('korresp'): all colummns must be numerical\n",
         call. = TRUE)
  
  if (!is.null(param.args$type) && param.args$type == "relational" && 
      (!identical(x.data, t(x.data)) || (sum(diag(x.data) != 0, na.rm = TRUE) > 0)))
    stop("data do not match chosen SOM type ('relational')\n", call. = TRUE)
  
  # Default dimension: nb.obs/10 with minimum equal to 5 and maximum to 10
  if (is.null(param.args$dimension)) {
    if (!is.null(param.args$type) && param.args$type=="korresp")
      param.args$dimension <- 
        c(max(5, min(10, ceiling(sqrt((nrow(x.data) + ncol(x.data)) / 10)))), 
          max(5, min(10, ceiling(sqrt((nrow(x.data) + ncol(x.data)) / 10)))))
    else
      param.args$dimension <- c(max(5, min(10, ceiling(sqrt(nrow(x.data) / 10)))), 
                                max(5, min(10, ceiling(sqrt(nrow(x.data) / 10)))))
  }
  
  # Default maxit: nb.obs*5
  if (is.null(param.args$maxit)) {
    if (!is.null(param.args$type) && param.args$type == "korresp") {
      param.args$maxit <- round((nrow(x.data) + ncol(x.data)) * 5)
    } else param.args$maxit <- round(nrow(x.data) * 5)
  }

  # Initialize parameters and print
  parameters <- do.call("initSOM", param.args)
  if (parameters$verbose) {
    cat("Self-Organizing Map algorithm...\n")
    print.paramSOM(parameters)
  }
  
  # Check missing values
  if (any(is.na(x.data)) && parameters$type != "numeric")
    stop("Missing values not allowed for non numeric SOM")
  
  nbna.row <- apply(x.data, 1, function(arow) sum(!is.na(arow)))
  if (any(nbna.row == 0))
    stop("Some individuals have completely missing entries SOM can not be trained.")
  
  nb.proto <- prod(parameters$the.grid$dim)
  
  ## Step 2: Preprocess the data
  # Scaling
  ## TODO: add tolerance
  norm.x.data <- preprocessData(x.data, parameters$scaling)
  if (parameters$type == "numeric" & ncol(norm.x.data) != ncol(x.data)) {
    x.data <- x.data[, colnames(norm.x.data)]
  }
  
  ## Step 3: Initialize prototypes
  # Check proto0 also now that the parameters have been initialized
  if (!is.null(parameters$proto0)) {
    if ((parameters$type == "korresp") &&
        (!identical(dim(parameters$proto0),
                    as.integer(c(nb.proto, ncol(x.data) + nrow(x.data)))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
           in the current SOM, prototypes must have ", 
           prod(parameters$the.grid$dimension), " rows and ", 
           ncol(x.data) + nrow(x.data), " columns\n", call. = TRUE)
    } else if (!identical(dim(parameters$proto0),
                          as.integer(c(nb.proto, ncol(x.data))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
           in the current SOM, prototypes must have ", 
           nb.proto, " rows and ",  ncol(x.data), " columns\n", call. = TRUE)
    }
    
    prototypes <- preprocessProto(parameters$proto0, parameters$scaling, x.data)
    
  } else {
    prototypes <- initProto(parameters, norm.x.data)
  }
  
  ## Step 4: Initialize distances matrix if needed
  if (parameters$type == "relational") {
    B <- norm.x.data %*% t(prototypes)
    A <- diag(prototypes %*% B)
    lambda <- rep(0, length = nrow(prototypes))
  }
  
  # Step 5: Initialize backup if needed
  if (parameters$nb.save == 1) {
    warning("'nb.save' can not be 1\n No intermediate backups saved",
            immediate. = TRUE, call. = TRUE)
  }
  
  if (parameters$nb.save > 1) {
    backup <- list()
    backup$prototypes <- list()
    backup$clustering <- matrix(ncol = parameters$nb.save, 
                                nrow = nrow(norm.x.data))
    backup$energy <- vector(length = parameters$nb.save)
    backup$steps <- round(seq(1, parameters$maxit, length=parameters$nb.save), 0)
  }
  
  ## Main Loop: from 1 to parameters$maxit
  for (ind.t in 1:parameters$maxit) {
    if (parameters$verbose) {
      if (ind.t %in% round(seq(1, parameters$maxit, length = 11))) {
        index <- match(ind.t, round(seq(1, parameters$maxit, length = 11)))
        cat((index - 1) * 10, "% done\n")
      }
    }
    
    ## Step 6: Randomly choose an observation
    rand.ind <- selectObs(ind.t, dim(x.data), parameters$type)
    sel.obs <- norm.x.data[rand.ind, ]
    
    ## Step 7: Assignment step
    # For the "korresp" type, cut the prototypes and selected observation
    if (parameters$type == "korresp") {
      if (ind.t %%2 == 0) { # sampling is on rows
        cur.obs <- sel.obs[1:ncol(x.data)]
        cur.prototypes <- prototypes[ ,1:ncol(x.data)]
      } else { # sampling is on columns
        cur.obs <- sel.obs[(ncol(x.data) + 1):ncol(norm.x.data)]
        cur.prototypes <- prototypes[ ,(ncol(x.data) + 1):ncol(norm.x.data)]
      }
    } else { # including missing data management: remove corresponding entries
      cur.prototypes <- prototypes
      cur.obs <- sel.obs
    }
    # radius value
    radius <- calculateRadius(parameters$the.grid, parameters$radius.type, 
                              ind.t, parameters$maxit)
    # assign current observation
    if (parameters$type == "relational" & (parameters$affectation == "standard")) {
      # fast computation
      winner <- which.min(B[rand.ind, ] - 0.5 * A)
    } else {
      if (parameters$type == "relational") {
        the.dist <- B[rand.ind, ] - 0.5 * A
      } else the.dist <- NULL
      winner <- oneObsAffectation(cur.obs, cur.prototypes, parameters$type,
                                  parameters$affectation, norm.x.data, 
                                  parameters$radius.type, radius, 
                                  parameters$the.grid, the.dist)
    }
    
    ## Step 8: Representation step
    the.nei <- selectNei(winner, parameters$the.grid, radius, 
                         radius.type = parameters$radius.type,
                         dist.type = parameters$the.grid$dist.type)
    
    epsilon <- 0.3 * parameters$eps0 / (1 + 0.2 * ind.t / nb.proto)
    
    if (parameters$type == "relational") {
      # compute lambda for fast update
      if (parameters$radius.type != "letremy") {
        lambda <- as.vector(epsilon * the.nei)
      } else {
        lambda <- rep(0, length = nrow(prototypes))
        lambda[the.nei] <- epsilon
      }
      # update distances matrix
      A <- (1 - lambda)^2 * A + lambda^2 * norm.x.data[rand.ind,rand.ind] + 
        2 * lambda * (1 - lambda) * B[rand.ind, ]
      B <- sweep(B, 2, 1 - lambda, "*") + 
        outer(norm.x.data[ ,rand.ind], lambda, "*")
    }
    
    # Update
    if (parameters$type == "relational") sel.obs <- rand.ind # only index is used
    prototypes <- prototypeUpdate(parameters$type, the.nei, epsilon, prototypes,
                                  sel.obs, parameters$radius.type, 
                                  winner = winner)
    
    ## Step 9: Intermediate backups (if needed)
    if (parameters$nb.save > 1) {
      if (ind.t %in% backup$steps) {
        
        out.proto <- dePreprocessProto(prototypes, parameters$scaling, x.data)
        colnames(out.proto) <- colnames(norm.x.data)
        rownames(out.proto) <- 1:nb.proto
        
        res <- list("parameters" = parameters, "prototypes" = out.proto, 
                    "data" = x.data)
        class(res) <- "somRes"
        
        ind.s <- match(ind.t, backup$steps)
        backup$prototypes[[ind.s]] <- out.proto
        if (parameters$type == "relational") {
          backup$clustering[, ind.s] <- predictRSOM(res, radius = radius, A = A,
                                                    B = B)
        } else backup$clustering[, ind.s] <- predict.somRes(res, radius = radius)

        backup$energy[ind.s] <- calculateEnergy(norm.x.data,
                                                backup$clustering[, ind.s],
                                                prototypes, parameters, ind.t)
      }
      
      if (ind.t == parameters$maxit) {
        clustering <- backup$clustering[, ind.s]
        if (parameters$type=="korresp") {
          names(clustering) <- c(colnames(x.data), rownames(x.data))
        } else names(clustering) <- rownames(x.data)
        energy <- backup$energy[ind.s]
      }
      
    } else if (ind.t == parameters$maxit) { # final state
      
      out.proto <- dePreprocessProto(prototypes, parameters$scaling, x.data)
      
      res <- list("parameters" = parameters, "prototypes" = out.proto,
                  "data" = x.data)
      class(res) <- "somRes"
      
      if (parameters$type=="relational") {
        clustering <- predictRSOM(res, A = A, B = B)
      } else clustering <- predict.somRes(res)
      
      if (parameters$type == "korresp") {
        names(clustering) <- c(colnames(x.data), rownames(x.data))
      } else names(clustering) <- rownames(x.data)
      
      energy <- calculateEnergy(norm.x.data, clustering, prototypes, parameters,
                                ind.t)
    }
  }
  
  colnames(out.proto) <- colnames(norm.x.data)
  rownames(out.proto) <- 1:nb.proto
  if (parameters$nb.save <= 1) { # no backup
    res <- list("clustering" = clustering, "prototypes" = out.proto,
                "energy" = energy, "data" = x.data, "parameters" = parameters)
  } else { # backup
    if (parameters$type == "korresp") {
      rownames(backup$clustering) <- c(colnames(x.data), rownames(x.data))
    } else rownames(backup$clustering) <- rownames(x.data)
    
    res <- list("clustering" = clustering, "prototypes" = out.proto,
                "energy" = energy, "backup" = backup, "data" = x.data, 
                "parameters" = parameters)
  }
  class(res) <- "somRes"
  
  return(res)
}

## S3 methods for somRes class objects ########################################

#' @export
#' @rdname trainSOM
print.somRes <- function(x, ...) {
  cat("      Self-Organizing Map object...\n")
  cat("        ", x$parameters$mode, "learning, type:", x$parameters$type,"\n")
  cat("        ", x$parameters$the.grid$dim[1],"x",
      x$parameters$the.grid$dim[2],
      "grid with",x$parameters$the.grid$topo, "topology\n")
  cat("         neighbourhood type:", x$parameters$radius.type,"\n")
  cat("         distance type:", x$parameters$the.grid$dist.type,"\n")
}

#' @method summary somRes
#' @export
#' @rdname trainSOM
summary.somRes <- function(object, ...) {
  cat("\nSummary\n\n")
  cat("      Class : ", class(object),"\n\n")
  print(object)
  cat("\n      Final energy     :", object$energy,"\n")
  error.topo <- quality(object, "topographic")
  cat("      Topographic error:", error.topo, "\n")
  if (object$parameters$type=="numeric") {
    cat("\n      ANOVA                : \n")
    res.anova <- as.data.frame(t(sapply(1:ncol(object$data), function(ind) {
      c(round(summary(aov(object$data[,ind] ~ as.factor(object$clustering)))[[1]][1, 4],
              digits = 3),
        round(summary(aov(object$data[,ind] ~ as.factor(object$clustering)))[[1]][1, 5],
              digits = 8))
    })))
    names(res.anova) <- c("F", "pvalue")
    res.anova$significativity <- rep("", ncol(object$data))
    res.anova$significativity[res.anova$"pvalue" < 0.05] <- "*"
    res.anova$significativity[res.anova$"pvalue" < 0.01] <- "**"
    res.anova$significativity[res.anova$"pvalue" < 0.001] <- "***"
    rownames(res.anova) <- colnames(object$data)
    
    cat("\n        Degrees of freedom : ", 
        summary(aov(object$data[,1] ~ as.factor(object$clustering)))[[1]][1,1],
        "\n\n")
    print(res.anova)  
    cat("\n")
  } else if (object$parameters$type == "korresp") {
    chisq.res <- chisq.test(object$data)
    sig <- ""
    if (chisq.res$p.value < 0.05) sig <- "*"
    if (chisq.res$p.value < 0.01) sig <- "**"
    if (chisq.res$p.value < 0.001) sig <- "***"
    cat("\n     ", chisq.res$method, ":\n\n")
    cat("         X-squared               : ", chisq.res$statistic, "\n")
    cat("         Degrees of freedom      : ", chisq.res$parameter, "\n")
    cat("         p-value                 : ", chisq.res$p.value, "\n")
    cat("                 significativity : ", sig, "\n")
  } else if (object$parameters$type == "relational") {
    if (object$parameters$scaling == "cosine") {
      norm.data <- preprocessData(object$data, object$parameters$scaling)
    } else norm.data <- object$data
    sse.total <- sum(norm.data) / (2*nrow(norm.data))
    
    sse.within <- sum(sapply(unique(object$clustering), function(clust)
      sum(norm.data[object$clustering == clust,object$clustering == clust]) /
        (2 * sum(object$clustering == clust))))
    
    n.clusters <- length(unique(object$clustering))
    F.stat <- ((sse.total - sse.within) / sse.within) * 
      ((nrow(norm.data) - n.clusters) / (n.clusters - 1))
    
    p.value <- 1 - pf(F.stat, n.clusters - 1, nrow(norm.data) - n.clusters)
    sig <- ""
    if (p.value < 0.001) {
      sig <- "***"
    } else if (p.value < 0.1) {
      sig <- "**"
    } else if (p.value < 0.05) sig <- "*"
    
    cat("\n      ANOVA            : \n")
    cat("         F                       : ", F.stat, "\n")
    cat("         Degrees of freedom      : ", n.clusters - 1, "\n")
    cat("         p-value                 : ", p.value, "\n")
    cat("                 significativity : ", sig, "\n")
  } 
}

#' @title Compute distances between prototypes
#' @export
#' @name protoDist
#' @aliases protoDist.somRes
#' 
#' @description Compute distances, either between all prototypes 
#' (\code{mode = "complete"}) or only between prototypes' neighbours 
#' (\code{mode = "neighbors"}).
#' 
#' @param object a \code{somRes} object.
#' @param mode Specifies which distances should be computed (default to 
#' \code{"complete"}).
#' @param radius Radius used to fetch the neighbors (default to 1). The distance
#' used to compute the neighbors is the Euclidean distance.
#' @param \dots Not used.
#' 
#' @details When \code{mode="complete"}, distances between all prototypes are
#' computed. When \code{mode="neighbors"}, distances are computed only between 
#' the prototypes and their neighbors. If the data were preprocessed during the
#' SOM training procedure, the distances are computed on the normalized values 
#' of the prototypes.
#' 
#' @return When \code{mode = "complete"}, the function returns a square matrix 
#' which dimensions are equal to the product of the grid dimensions.
#' 
#' When \code{mode = "neighbors"}, the function returns a list which length is 
#' equal to the product of the grid dimensions; the length of each item is equal
#' to the number of neighbors. Neurons are considered to have 8 neighbors at 
#' most (\emph{i.e.}, two neurons are neighbors if they have an Euclidean 
#' distance smaller than \code{radius}. Natural choice for \code{radius} is
#' 1 for hexagonal topology and 1 or \eqn{\sqrt{2}}{sqrt(2)} for square 
#' topology (4 and 8 neighbors respectively).
#' 
#' @author Madalina Olteanu \email{olteanu@ceremade.dauphine.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{trainSOM}}
#' 
#' @examples
#' set.seed(2343)
#' my.som <- trainSOM(x.data = iris[,1:4], dimension = c(5,5))
#' protoDist(my.som)

protoDist <- function(object, mode = c("complete", "neighbors"), radius = 1, ...) {
  UseMethod("protoDist")
}

#' @export

## TODO: improve code below
protoDist.somRes <- function(object, mode = c("complete", "neighbors"), 
                             radius = 1, ...) {
  mode <- match.arg(mode)
  complete <- (mode == "complete")
  prototypes <- preprocessProto(object$prototypes, object$parameters$scaling, 
                                object$data)
  if (object$parameters$type == "relational") {
    x.data <- preprocessData(object$data, object$parameters$scaling)
  } else x.data <- NULL
  
  the.grid <- object$parameters$the.grid
  type <- object$parameters$type
  
  if (!complete) {
    all.nei <- sapply(1:prod(the.grid$dim), selectNei, the.grid = the.grid,
                      radius = radius, radius.type = "letremy", 
                      dist.type = "euclidean")
    all.nei <- sapply(1:prod(the.grid$dim), function(neuron) 
      setdiff(all.nei[[neuron]], neuron))
    if (type != "relational") {# euclidean case
      distances <- sapply(1:prod(the.grid$dim), function(one.neuron) {
        apply(prototypes[all.nei[[one.neuron]], ], 1, distEuclidean,
              y = prototypes[one.neuron, ])
      })
    } else {
      distances <- sapply(1:prod(the.grid$dim), function(one.neuron) {
        apply(prototypes[all.nei[[one.neuron]],], 1, distRelationalProto,
              proto2 = prototypes[one.neuron,], x.data = x.data)
      })
      if (sum(unlist(distances) < 0) > 0)
        warning("some of the relational 'distances' are negatives\n
                plots, qualities, super-clustering... may not work!",
                immediate. = TRUE, call. = TRUE)
    }
  } else {
    if (type == "relational") {# non euclidean case
      distances <- apply(prototypes, 1, function(one.proto) {
        apply(prototypes, 1, distRelationalProto, proto2 = one.proto,
              x.data = x.data)
      })
      if (sum(distances < 0) > 0)
        warning("some of the relational 'distances' are negatives\n
                plots, qualities, super-clustering... may not work!",
                immediate. = TRUE, call. = TRUE)
    } else distances <- as.matrix(dist(prototypes, upper = TRUE, diag = TRUE))
  }
  
  return(distances)
}