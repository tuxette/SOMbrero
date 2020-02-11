## SOM algorithm functions
## ----------------------------------------------------------------------------

##### Auxiliary functions
################################################################################

# Cosine preprocessing (for "relational" case)
cosinePreprocess <- function(diss.matrix, x.new= NULL, tolerance=10^(-10)) {
  # tolerance is used to solve numeric instabilities
  # similarity matrix by double centering
  sim.matrix <- -.5* (diag(1, nrow(diss.matrix))- 1/nrow(diss.matrix)) %*%
    diss.matrix %*% (diag(1, nrow(diss.matrix))-1/nrow(diss.matrix))
  sim.matrix <- round(sim.matrix, -log10(tolerance))
  
  if (is.null(x.new)) {
    # normalize the original dissimilarity matrix
    scaled.ker <- sweep(sweep(sim.matrix,2,sqrt(diag(sim.matrix)),"/"),
                        1,sqrt(diag(sim.matrix)),"/")
    # came back to dissimilarity
    # if cosine preprocess is useless it is the original dissimilarity
    scaled.diss <- 2-2*scaled.ker
    rownames(scaled.diss) <- rownames(diss.matrix)
    colnames(scaled.diss) <- colnames(diss.matrix)
    scaled.diss <- round(scaled.diss, -log10(tolerance))
  } else {
    # normalize additional dissimilarity lines like the similarity matrix
    sim.x <- t(apply(x.new, 1, function(x) 
      -.5*(x - mean(x) - colMeans(diss.matrix) + mean(diss.matrix))))
    sim.x <- round(sim.x, -log10(tolerance))
    auto.sim <- round(apply(x.new,1,mean) - mean(diss.matrix)/2,
                      -log10(tolerance))
    if (nrow(x.new)>1) {
      scaled.ker <- sweep(sweep(sim.x, 2, sqrt(diag(sim.matrix)), "/"), 1,
                          sqrt(auto.sim), "/")
    } else {
      scaled.ker <- sweep(sim.x, 2, sqrt(diag(sim.matrix)), "/")/
        sqrt(auto.sim)
    }
    scaled.diss <- 2-2*scaled.ker
    colnames(scaled.diss) <- colnames(diss.matrix)
    rownames(scaled.diss) <- rownames(x.new)
    scaled.diss <- round(scaled.diss, -log10(tolerance))
  }
  return(scaled.diss)
}

# Preprocess data or prototypes
preprocessData <- function(x.data, scaling) {
  switch(scaling,
         "unitvar"=scale(x.data, center=TRUE, scale=TRUE),
         "center"=scale(x.data, center=TRUE, scale=FALSE),
         "none"=as.matrix(x.data),
         "chi2"=korrespPreprocess(x.data),
         "cosine"=cosinePreprocess(x.data))
}

preprocessProto <- function(prototypes, scaling, x.data) {
  switch(scaling,
         "unitvar"=scale(prototypes, 
                         center=apply(x.data,2,mean),
                         scale=apply(x.data,2,sd)),
         "center"=scale(prototypes, 
                        center=apply(x.data,2,mean),
                        scale=FALSE),
         "none"=prototypes,
         "chi2"=prototypes,
         "cosine"=prototypes)
}

calculateRadius <- function(the.grid, radius.type, ind.t, maxit) {
  # ind.t: iteration index
  if (radius.type=="letremy") {
    r0 <- max(floor(the.grid$dim/2))
    k <- 4*(r0-1)/maxit
    a <- floor(maxit/2)
    b <- floor(maxit*3/4)
    r <- ceiling(r0/(1+k*ind.t))
    if (ind.t==1) {
      r <- r0
    } else if (ind.t>=a & ind.t<b) {
      r <- 0.5
    } else if (ind.t>=b) {
      r <- 0
    }
  } else if (radius.type=="gaussian") {
    if (the.grid$topo=="square")
      r0 <- 1+(2/3)*sqrt(sum(the.grid$dim^2))
    r <- r0*(1/r0)^((ind.t-1)/(maxit-1))-1 # power decrease 
  }
  
  r
}

selectNei <- function(the.neuron, the.grid, radius, radius.type, 
                      dist.type=the.grid$dist.type) {
  if (radius.type=="letremy") {
    if (dist.type=="letremy") {
      if (radius==0.5) {
        the.dist <- as.matrix(dist(the.grid$coord, diag=TRUE, upper=TRUE,
                                   method="euclidean"))[the.neuron,]
        the.nei <- which(the.dist<=1)
      } else {
        the.dist <- as.matrix(dist(the.grid$coord, diag=TRUE, upper=TRUE,
                                   method="maximum"))[the.neuron,]
        the.nei <- which(the.dist<=radius)
      }
    } else {
      the.dist <- as.matrix(dist(the.grid$coord, diag=TRUE, upper=TRUE,
                                 method=dist.type))[the.neuron,]
      the.nei <- which(the.dist<=radius)
    }
  } else if (radius.type=="gaussian") {
    proto.dist <- as.matrix(dist(the.grid$coord, upper=TRUE, 
                                 diag=TRUE, method=dist.type))
    sigma <- quantile(proto.dist[upper.tri(proto.dist)]^2, 0.5)
    the.nei <- exp(-proto.dist[the.neuron,]^2/sigma/(radius+1)^2)
  }
  
  the.nei
}

# Functions to manipulate objects in the input space
distEuclidean <- function(x,y) {
  sqrt(sum((x-y)^2))
}

distRelationalProto <- function(proto1, proto2, x.data) {
  -0.5*t(proto1-proto2)%*%x.data%*%(proto1-proto2)
}

calculateProtoDist <- function(prototypes, the.grid, type, complete=FALSE,
                               x.data=NULL) {
  if (!complete) {
    all.nei <- sapply(1:prod(the.grid$dim), selectNei, the.grid=the.grid,
                      radius=1, radius.type="letremy", dist.type="letremy")
    all.nei <- sapply(1:prod(the.grid$dim), function(neuron) 
      setdiff(all.nei[[neuron]], neuron))
    if (type!="relational") {# euclidean case
      distances <- sapply(1:prod(the.grid$dim), function(one.neuron) {
        apply(prototypes[all.nei[[one.neuron]],],1,distEuclidean,
              y=prototypes[one.neuron,])
      })
    } else {
      distances <- sapply(1:prod(the.grid$dim), function(one.neuron) {
        apply(prototypes[all.nei[[one.neuron]],],1,distRelationalProto,
              proto2=prototypes[one.neuron,], x.data=x.data)
      })
      if (sum(unlist(distances)<0)>0)
        warning("some of the relational 'distances' are negatives\n
                plots, qualities, super-clustering... may not work!",
                immediate.=TRUE, call.=TRUE)
    }
  } else {
    if (type=="relational") {# non euclidean case
      distances <- apply(prototypes,1,function(one.proto) {
        apply(prototypes, 1, distRelationalProto, proto2=one.proto,
              x.data=x.data)
      })
      if (sum(distances<0)>0)
        warning("some of the relational 'distances' are negatives\n
                plots, qualities, super-clustering... may not work!",
                immediate.=TRUE, call.=TRUE)
    } else distances <- as.matrix(dist(prototypes, upper=TRUE, diag=TRUE))
  }
  
  distances
  }

## Functions used during training of SOM
# Step 2: Preprocess data ("korresp" case)
korrespPreprocess <- function(cont.table) {
  both.profiles <- matrix(0, nrow=nrow(cont.table)+ncol(cont.table),
                          ncol=ncol(cont.table)+nrow(cont.table))
  # row profiles
  both.profiles[1:nrow(cont.table), 1:ncol(cont.table)] <-
    cont.table/outer(sqrt(rowSums(cont.table)), 
                     sqrt(colSums(cont.table)/sum(cont.table)))  
  # column profiles
  both.profiles[(nrow(cont.table)+1):(nrow(cont.table)+ncol(cont.table)),
                (ncol(cont.table)+1):(ncol(cont.table)+nrow(cont.table))] <- 
    t(cont.table)/outer(sqrt(colSums(cont.table)), 
                        sqrt(rowSums(cont.table)/sum(cont.table)))
  # Best column to complete row profiles
  best.col <- apply(both.profiles[1:nrow(cont.table), 1:ncol(cont.table)],
                    1,which.max)
  both.profiles[1:nrow(cont.table), (ncol(cont.table)+1):ncol(both.profiles)] <- 
    both.profiles[best.col+nrow(cont.table),
                  (ncol(cont.table)+1):ncol(both.profiles)]
  # Best row to complete col profiles
  best.row <- apply(both.profiles[(nrow(cont.table)+1):
                                    (nrow(cont.table)+ncol(cont.table)),
                                  (ncol(cont.table)+1):
                                    (ncol(cont.table)+nrow(cont.table))],
                    1,which.max)
  both.profiles[(nrow(cont.table)+1):(nrow(cont.table)+ncol(cont.table)),
                1:ncol(cont.table)] <-
    both.profiles[best.row, 1:ncol(cont.table)]
  # Names
  rownames(both.profiles) <- c(rownames(cont.table),colnames(cont.table))
  colnames(both.profiles) <- c(colnames(cont.table),rownames(cont.table))
  return(both.profiles)
}

# Step 3: Initialize prototypes
initProto <- function(parameters, norm.x.data, x.data) {
  if (is.null(parameters$proto0)) {
    if (parameters$init.proto=="random") {
      if (parameters$type=="relational") {
        prototypes <- t(apply(matrix(runif(prod(parameters$the.grid$dim, 
                                                nrow(norm.x.data))), 
                                     nrow=prod(parameters$the.grid$dim)),
                              1, function(x)x/sum(x)))
      } else {
        # both numeric and korresp
        prototypes <- sapply(1:ncol(norm.x.data),
                             function(ind){
                               runif(prod(parameters$the.grid$dim),
                                     min=min(norm.x.data[,ind]),
                                     max=max(norm.x.data[,ind]))})
      }
    } else if (parameters$init.proto=="obs") {
      if (parameters$type=="korresp"|parameters$type=="numeric") {
        prototypes <- norm.x.data[sample(1:nrow(norm.x.data), 
                                         prod(parameters$the.grid$dim), 
                                         replace=TRUE),]
      } else if (parameters$type=="relational") {
        prototypes <- matrix(0, nrow=prod(parameters$the.grid$dim),
                             ncol=ncol(norm.x.data))
        prototypes[cbind(1:nrow(prototypes),
                         sample(1:ncol(prototypes),nrow(prototypes),
                                replace=TRUE))] <- 1
      }
    } else if (parameters$init.proto=="pca") {
      # the most detailed grid axis is assigned to the first component
      if (parameters$the.grid$dim[1]>=parameters$the.grid$dim[2]) {
        x.ev <- 1
        y.ev <- 2
      } else {
        x.ev <- 2
        y.ev <- 1
      }
      if (parameters$type=="numeric") {
        # perform PCA
        data.pca <- princomp(norm.x.data)
        x <- seq(from=quantile(data.pca$scores[,x.ev], .025), 
                 to=quantile(data.pca$scores[,x.ev], .975),
                 length.out=parameters$the.grid$dim[1])
        y <- seq(from=quantile(data.pca$scores[,y.ev], .025), 
                 to=quantile(data.pca$scores[,y.ev], .975),
                 length.out=parameters$the.grid$dim[2])
        base <- as.matrix(expand.grid(x=x, y=y))
        # search for the closest observation
        closest.obs <- apply(base, 1, function(point) 
          which.min(colSums((t(data.pca$scores[,c(x.ev,y.ev)])-point)^2)))
        prototypes <- matrix(0, ncol=nrow(norm.x.data), 
                             nrow=prod(parameters$the.grid$dim))
        prototypes <- norm.x.data[closest.obs,]
      } else if (parameters$type=="relational") {
        data.mds <- cmdscale(norm.x.data)
        x <- seq(from=quantile(data.mds[,x.ev], .025), 
                 to=quantile(data.mds[,x.ev], .975),
                 length.out=parameters$the.grid$dim[1])
        y <- seq(from=quantile(data.mds[,y.ev], .025), 
                 to=quantile(data.mds[,y.ev], .975),
                 length.out=parameters$the.grid$dim[2])
        base <- as.matrix(expand.grid(x=x, y=y))
        closest.obs <- apply(base, 1, function(point) 
          which.min(colSums((t(data.mds[,1:2])-point)^2)))
        prototypes <- matrix(0, ncol=nrow(norm.x.data), 
                             nrow=prod(parameters$the.grid$dim))
        prototypes[cbind(1:prod(parameters$the.grid$dim),closest.obs)] <- 1
      }
    }
  } else {
    prototypes <- preprocessProto(parameters$proto0, parameters$scaling,
                                  x.data)
  }
  return(prototypes)
}

# Step 5: Randomly choose an observation
selectObs <- function(ind.t, ddim, type) {
  if (type=="korresp") {
    if (ind.t%%2==0) {
      rand.ind <- sample(1:ddim[1],1)
    } else rand.ind <- sample((ddim[1]+1):(ddim[1]+ddim[2]),1)
  } else rand.ind <- sample(1:ddim[1],1)
  return(rand.ind) 
}

# Step 6: Assignment step
oneObsAffectation <- function(x.new, prototypes, type, affectation, x.data=NULL,
                              radius.type=NULL, radius=NULL, the.grid=NULL) {
  if (affectation=="standard") {
    # kept in case new data is added in the model
    if (type=="relational") {
      the.neuron <- which.min(prototypes%*%x.new-
                                0.5*diag(prototypes%*%x.data%*%
                                           t(prototypes)))
    } else the.neuron <- which.min(apply(prototypes, 1, distEuclidean, y=x.new))
  } else {# Heskes's soft affectation
    # kept in case new data is added in the model
    if (type=="relational") {
      if (radius.type!="letremy") {
        the.dist <- prototypes%*%x.new-0.5*diag(prototypes%*%x.data%*%
                                                  t(prototypes))
        final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
          the.nei <- selectNei(a.neuron, the.grid, radius, radius.type,
                               the.grid$dist.type)
          return(sum(the.dist*the.nei))
        })
        the.neuron <- which.min(final.dist)
      } else {
        the.dist <- prototypes%*%x.new-0.5*diag(prototypes%*%x.data%*%
                                                  t(prototypes))
        final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
          the.nei <- selectNei(a.neuron, the.grid, radius, radius.type,
                               the.grid$dist.type)
          return(sum(the.dist[the.nei]))
        })
        the.neuron <- which.min(final.dist)
      }
    } else {
      if (radius.type!="letremy") {
        the.dist <- apply(prototypes, 1, distEuclidean, y=x.new)
        final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
          the.nei <- selectNei(a.neuron, the.grid, radius, radius.type,
                               the.grid$dist.type)
          return(sum(the.dist*the.nei))
        })
        the.neuron <- which.min(final.dist)
      } else {
        the.dist <- apply(prototypes, 1, distEuclidean, y=x.new)
        final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
          the.nei <- selectNei(a.neuron, the.grid, radius, radius.type,
                               the.grid$dist.type)
          return(sum(the.dist[the.nei]))
        })
        the.neuron <- which.min(final.dist)
      }
    }
  }
  
  the.neuron
}

obsAffectation <- function(x.new, prototypes, type, affectation, x.data=NULL,
                           radius.type=NULL, radius=NULL, the.grid=NULL) {
  if (is.null(dim(x.new)) || nrow(x.new) == 1) {
    if (!is.null(dim(x.new))) x.new <- x.new[1, ]
    the.neuron <- oneObsAffectation(x.new, prototypes, type, affectation, 
                                    x.data, radius.type, radius, the.grid)
  } else {
    # distance between all prototypes and all data
    # kept in case new data is added in the model
    if (type=="relational") {
      dist.1 <- -0.5*diag(prototypes%*%x.data%*%t(prototypes))
      dist.2 <- tcrossprod(prototypes, x.new)
      all.dist <- sweep(dist.2, 1, dist.1, "+")
    } else {
      # Euclidean distance
      dist.1 <- -2*tcrossprod(prototypes, x.new)
      dist.2 <- diag(tcrossprod(prototypes, prototypes))
      all.dist <- sweep(dist.1, 1, dist.2, "+")
    }
    
    # affectation to the closest prototype
    if (affectation=="standard") {
      the.neuron <- apply(all.dist, 2, which.min)
    } else {
      # Heskes's soft affectation
      u.weights <- sapply(1:nrow(prototypes), function(a.neuron) {
        the.nei <- selectNei(a.neuron, the.grid, radius, radius.type,
                             the.grid$dist.type)
        return(the.nei)
      })
      if (type != "relational")
        all.dist <- sweep(all.dist, 2, apply(x.new^2, 1, sum), "+")
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
          w.dist <- matrix(unlist(w.dist), nrow=25, byrow=TRUE)
        }
      }
      the.neuron <- apply(w.dist, 2, which.min)
    }
  }
  
  the.neuron
}

# Step 7: Update of prototypes
prototypeUpdate <- function(type, the.nei, epsilon, prototypes, rand.ind,
                            sel.obs, radius.type) {
  if (radius.type!="letremy") {
    if (type=="relational") {
      indic <- matrix(0, nrow=nrow(prototypes), ncol=ncol(prototypes)) 
      indic[,rand.ind] <- 1
      prototypes <- (1-epsilon*the.nei)*prototypes + epsilon*the.nei*indic    
    } else {
      prototypes <- (1-epsilon*the.nei)*prototypes + 
        epsilon*the.nei*outer(rep(1,nrow(prototypes)), sel.obs)
    }
  } else {
    if (type=="relational") {
      indic <- matrix(0,nrow=length(the.nei),ncol=ncol(prototypes)) 
      indic[,rand.ind] <- 1
      prototypes[the.nei,] <- (1-epsilon)*prototypes[the.nei,] + epsilon*indic    
    } else {
      prototypes[the.nei,] <- (1-epsilon)*prototypes[the.nei,] +
        epsilon*outer(rep(1,length(the.nei)), sel.obs)
    }
  }
  return(prototypes)
}

# Step 8: calculate intermediate energy
# TODO: It would probably be better to implement a function 'distEltProto'
calculateClusterEnergy <- function(cluster, x.data, clustering, prototypes,
                                   parameters, radius) {
  the.nei <- selectNei(cluster, parameters$the.grid, radius, 
                       parameters$radius.type, parameters$the.grid$dist.type)
  if (parameters$radius.type=="letremy")
    the.nei <- as.numeric((1:nrow(prototypes))%in%the.nei)
  
  if (parameters$type=="numeric" || parameters$type=="korresp") {
    return(sum(the.nei[clustering]*
                 rowSums((x.data-outer(rep(1,nrow(x.data)), 
                                       prototypes[cluster,]))^2)))
  } else if (parameters$type=="relational") {
    return(sum(the.nei[clustering]*
                 (matrix(prototypes[cluster,], nrow=1)%*%x.data-
                    as.numeric(0.5*matrix(prototypes[cluster,], nrow=1)%*%
                                 x.data%*%matrix(prototypes[cluster,]))
                 )))
  }
  
}

calculateEnergy <- function(x.data, clustering, prototypes, parameters, ind.t) {
  radius <- calculateRadius(parameters$the.grid, parameters$radius.type,
                            ind.t, parameters$maxit)
  sum(unlist(sapply(1:nrow(prototypes), calculateClusterEnergy,
                    x.data=x.data, clustering=clustering, 
                    prototypes=prototypes, parameters=parameters,
                    radius=radius)))/nrow(x.data)/nrow(prototypes)
}

##### Main function
################################################################################

#' @title Run the SOM algorithm
#' @export
#' @name trainSOM
#' @exportClass somRes
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
#'   \item{clustering}{the final classification of the data.}
#'   \item{prototypes}{the final coordinates of the prototypes.}
#'   \item{energy}{the final energy of the map.}
#'   \item{backup}{a list containing some intermediate backups of the prototypes
#'   coordinates, clustering, energy and the indexes of the recorded backups, if
#'   \code{nb.save} is set to a value larger than 1.}
#'   \item{data}{the original dataset used to train the algorithm.}
#'   \item{parameters}{a list of the map's parameters, which is an object of 
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
#' described in Cottrell et al., 2004 (with weights as in Cottrel and Letremy, 
#' 2005); \code{type="relational"} for dissimilarity matrices, as described in
#' Olteanu et al., 2015, with the fast implementation introduced in Mariette
#' \emph{et al.}, 2017.
#' 
#' \code{summary} produces a complete summary of the results that displays the 
#' parameters of the SOM, quality criteria and ANOVA. For \code{type="numeric"}
#' the ANOVA is performed for each input variable and test the difference of 
#' this variable accross the clsuters of the map. For \code{type="relational"} a
#' dissimilarity ANOVA is performed (see (Anderson, 2001), except that in the 
#' present version, a crude estimate of the p-value is used which is based on 
#' the Fisher distribution and not on a permutation test.
#' 
#' @references 
#' Anderson M.J. (2001). A new method for non-parametric multivariate analysis 
#' of variance. \emph{Austral Ecology}, \strong{26}, 32-46. 
#' 
#' Kohonen T. (2001) \emph{Self-Organizing Maps}. Berlin/Heidelberg: 
#' Springer-Verlag, 3rd edition.
#' 
#' Cottrell M., Ibbou S., Letremy P. (2004) SOM-based algorithms for qualitative
#' variables. \emph{Neural Networks}, \strong{17}, 1149-1167.
#' 
#' Cottrell M., Letremy P. (2005) How to use the Kohonen algorithm to 
#' simultaneously analyse individuals in a survey. \emph{Neurocomputing}, 
#' \strong{21}, 119-138.
#' 
#' Olteanu M., Villa-Vialaneix N. (2015) On-line relational and multiple
#' relational SOM. \emph{Neurocomputing}, \strong{147}, 15-30. 
#' 
#' Mariette J., Rossi F., Olteanu M., Mariette J. (2017) Accelerating stochastic 
#' kernel SOM. In: M. Verleysen, \emph{XXVth European Symposium on Artificial 
#' Neural Networks, Computational Intelligence and Machine Learning 
#' (ESANN 2017)}, i6doc, Bruges, Belgium, 269-274.
#' 
#' @author Jérome Mariette \email{jerome.mariette@inrae.fr}\cr
#' Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Fabrice Rossi \email{fabrice.rossi@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#'
#' @note Warning! Recording intermediate backups with the argument 
#' \code{nb.save} can strongly increase the computational time since calculating
#' the entire clustering and the energy is time consumming. Use this option with
#' care and only when it is strictly necessary.
#' 
#' @seealso See \code{\link{initSOM}} for a description of the paramaters to 
#' pass to the trainSOM function to change its behavior and 
#' \code{\link{plot.somRes}} to plot the outputs of the algorithm.
#' 
#' @examples 
#' # Run trainSOM algorithm on the iris data with 500 iterations
#' iris.som <- trainSOM(x.data=iris[,1:4])
#' iris.som
#' summary(iris.som)

trainSOM <- function (x.data, ...) {
  param.args <- list(...)
  ## Step 1: Parameters handling
  if (!is.matrix(x.data)) x.data <- as.matrix(x.data, rownames.force=TRUE)
  
  # Default dimension: nb.obs/10 with minimum equal to 5 and maximum to 10
  if (is.null(param.args$dimension)) {
    if (!is.null(param.args$type) && param.args$type=="korresp")
      param.args$dimension <- 
        c(max(5,min(10,ceiling(sqrt((nrow(x.data)+ncol(x.data))/10)))), 
          max(5,min(10,ceiling(sqrt((nrow(x.data)+ncol(x.data))/10)))))
    else
      param.args$dimension <- c(max(5,min(10,ceiling(sqrt(nrow(x.data)/10)))), 
                                max(5,min(10,ceiling(sqrt(nrow(x.data)/10)))))
  }
  # Default maxit: nb.obs*5
  if (is.null(param.args$maxit)) {
    if (!is.null(param.args$type) && param.args$type=="korresp")
      param.args$maxit <- round((nrow(x.data)+ncol(x.data))*5)
    else
      param.args$maxit <- round(nrow(x.data)*5)
  }
  # Check inputs
  if (!is.null(param.args$type) && param.args$type=="relational" && 
      (!identical(x.data, t(x.data)) || (sum(diag(x.data)!=0)>0)))
    stop("data do not match chosen SOM type ('relational')\n", call.=TRUE)
  
  # Initialize parameters and print
  parameters <- do.call("initSOM", param.args)
  if (parameters$verbose) {
    cat("Self-Organizing Map algorithm...\n")
    print.paramSOM(parameters)
  }
  
  # Check proto0 also now that the parameters have been initialized
  if (!is.null(parameters$proto0)) {
    if ((parameters$type=="korresp")&&
        (!identical(dim(parameters$proto0),
                    as.integer(c(prod(parameters$the.grid$dimension),
                                 ncol(x.data)+nrow(x.data)))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
           in the current SOM, prototypes must have ", 
           prod(parameters$the.grid$dimension), " rows and ", 
           ncol(x.data)+nrow(x.data), " columns\n", call.=TRUE)
    } else if (!identical(dim(parameters$proto0),
                          as.integer(c(prod(parameters$the.grid$dimension),
                                       ncol(x.data))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
           in the current SOM, prototypes must have ", 
           prod(parameters$the.grid$dimension), " rows and ", 
           ncol(x.data), " columns\n", call.=TRUE)
    }
  }
  
  ## Step 2: Preprocess the data
  # Scaling
  norm.x.data <- preprocessData(x.data, parameters$scaling)
  
  ## Step 3: Initialize prototypes
  prototypes <- initProto(parameters, norm.x.data, x.data)
  
  ## Step 4: Initialize distances matrix if needed
  if (parameters$type=="relational") {
    B <- norm.x.data%*%t(prototypes)
    A <- diag(prototypes%*%B)
    lambda <- rep(0, length = nrow(prototypes))
  }
  
  # Step 5: Iitialize backup if needed
  if(parameters$nb.save>1) {
    backup <- list()
    backup$prototypes <- list()
    backup$clustering <- matrix(ncol=parameters$nb.save, 
                                nrow=nrow(norm.x.data))
    backup$energy <- vector(length=parameters$nb.save)
    backup$steps <- round(seq(1,parameters$maxit,length=parameters$nb.save),0)
  }
  
  ## Main Loop: from 1 to parameters$maxit
  for (ind.t in 1:parameters$maxit) {
    if (parameters$verbose) {
      if (ind.t %in% round(seq(1, parameters$maxit, length=11))) {
        index <- match(ind.t, round(seq(1, parameters$maxit, length=11)))
        cat((index-1)*10, "% done\n")
      }
    }
    
    ## Step 6: Randomly choose an observation
    rand.ind <- selectObs(ind.t, dim(x.data), parameters$type)
    sel.obs <- norm.x.data[rand.ind,]
    
    ## Step 7: Assignment step
    # For the "korresp" type, cut the prototypes and selected observation
    if (parameters$type=="korresp") {
      if (ind.t%%2==0) {
        cur.obs <- sel.obs[1:ncol(x.data)]
        cur.prototypes <- prototypes[,1:ncol(x.data)]
      } else {
        cur.obs <- sel.obs[(ncol(x.data)+1):ncol(norm.x.data)]
        cur.prototypes <- prototypes[,(ncol(x.data)+1):ncol(norm.x.data)]
      }
    } else {
      cur.prototypes <- prototypes
      cur.obs <- sel.obs
    }
    # Radius value
    radius <- calculateRadius(parameters$the.grid, parameters$radius.type,
                              ind.t, parameters$maxit)
    # Assign
    if (parameters$type=="relational") {
      if (parameters$affectation=="standard") {
        winner <- which.min(B[rand.ind,]-0.5*A)
      } else {# Heskes's soft affectation
        the.dist <- B[rand.ind,]-0.5*A
        if (parameters$radius.type!="letremy") {
          final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
            the.nei <- selectNei(a.neuron, parameters$the.grid, radius, 
                                 parameters$radius.type, parameters$the.grid$dist.type)
            return(sum(the.dist*the.nei))
          })
        } else {
          final.dist <- sapply(1:nrow(prototypes), function(a.neuron) {
            the.nei <- selectNei(a.neuron, parameters$the.grid, radius, 
                                 parameters$radius.type, parameters$the.grid$dist.type)
            return(sum(the.dist[the.nei]))
          })
        }
        winner <- which.min(final.dist)
      }
    } else {
      winner <- oneObsAffectation(cur.obs, cur.prototypes, parameters$type,
                                  parameters$affectation, norm.x.data, 
                                  parameters$radius.type, radius, 
                                  parameters$the.grid)
    }
    
    ## Step 8: Representation step
    the.nei <- selectNei(winner, parameters$the.grid, radius, 
                         radius.type=parameters$radius.type,
                         dist.type=parameters$the.grid$dist.type)
    
    epsilon <- 0.3*parameters$eps0/(1+0.2*ind.t/prod(parameters$the.grid$dim))
    
    if (parameters$type=="relational") {
      # compute lambda
      if (parameters$radius.type!="letremy") {
        lambda <- as.vector(epsilon*the.nei)
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
    prototypes <- prototypeUpdate(parameters$type, the.nei, epsilon, 
                                  prototypes, rand.ind, sel.obs,
                                  parameters$radius.type)
    
    ## Step 9: Intermediate backups (if needed)
    if (parameters$nb.save==1) {
      warning("nb.save can not be 1\n No intermediate backups saved",
              immediate.=TRUE, call.=TRUE)
    }
    if (parameters$nb.save>1) {
      if(ind.t %in% backup$steps) {
        out.proto <- switch(parameters$scaling,
                            "unitvar"=scale(prototypes, 
                                            center=-apply(x.data,2,mean)/
                                              apply(x.data,2,sd),
                                            scale=1/apply(x.data,2,sd)),
                            "center"=scale(prototypes, 
                                           center=-apply(x.data,2,mean),
                                           scale=FALSE),
                            "none"=prototypes,
                            "chi2"=prototypes,
                            "cosine"=prototypes)
        colnames(out.proto) <- colnames(norm.x.data)
        rownames(out.proto) <- 1:prod(parameters$the.grid$dim)
        res <- list("parameters"=parameters, "prototypes"=out.proto, 
                    "data"=x.data)
        class(res) <- "somRes"
        
        ind.s <- match(ind.t,backup$steps)
        backup$prototypes[[ind.s]] <- out.proto
        if (parameters$type=="relational") {
          backup$clustering[,ind.s] <- predictRSOM(res, radius=radius, A=A, B=B)
        } else backup$clustering[,ind.s] <- predict.somRes(res, radius=radius)
        backup$energy[ind.s] <- calculateEnergy(norm.x.data,
                                                backup$clustering[,ind.s],
                                                prototypes, parameters, ind.t)
      }
      if (ind.t==parameters$maxit) {
        clustering <- backup$clustering[,ind.s]
        if (parameters$type=="korresp") {
          names(clustering) <- c(colnames(x.data), rownames(x.data))
        } else names(clustering) <- rownames(x.data)
        energy <- backup$energy[ind.s]
      }
    } else if (ind.t==parameters$maxit) {
      out.proto <- switch(parameters$scaling,
                          "unitvar"=scale(prototypes, 
                                          center=-apply(x.data,2,mean)/
                                            apply(x.data,2,sd),
                                          scale=1/apply(x.data,2,sd)),
                          "center"=scale(prototypes, 
                                         center=-apply(x.data,2,mean),
                                         scale=FALSE),
                          "none"=prototypes,
                          "chi2"=prototypes,
                          "cosine"=prototypes)
      
      res <- list("parameters"=parameters, "prototypes"=out.proto,
                  "data"=x.data)
      class(res) <- "somRes"
      if (parameters$type=="relational") {
        clustering <- predictRSOM(res, A=A, B=B)
      } else clustering <- predict.somRes(res)
      if (parameters$type=="korresp") {
        names(clustering) <- c(colnames(x.data), rownames(x.data))
      } else names(clustering) <- rownames(x.data)
      energy <- calculateEnergy(norm.x.data, clustering, prototypes, parameters,
                                ind.t)
    }
  }
  
  colnames(out.proto) <- colnames(norm.x.data)
  rownames(out.proto) <- 1:prod(parameters$the.grid$dim)
  if (parameters$nb.save<=1) {
    res <- list("clustering"=clustering, "prototypes"=out.proto,
                "energy"=energy, "data"=x.data, "parameters"=parameters)
  } else {
    if (parameters$type=="korresp") {
      rownames(backup$clustering) <- c(colnames(x.data), rownames(x.data))
    } else rownames(backup$clustering) <- rownames(x.data)
    res <- list("clustering"=clustering, "prototypes"=out.proto,
                "energy"=energy, "backup"=backup, "data"=x.data, 
                "parameters"=parameters)
  }
  class(res) <- "somRes"
  return(res)
}

projectGraph <- function(the.graph, clustering, coord.clustering) {
  ## TODO: handle directed graph...
  ## If directed, convert into undirected
  if (is.directed(the.graph)) the.graph <- as.undirected(the.graph)
  
  all.neurons <- 1:nrow(coord.clustering)
  nonempty.neurons <- sort(unique(clustering))
  p.edges <- NULL # list of edges
  p.edges.weights <- NULL # weights of the edges
  if (is.null(V(the.graph)$size)) { # number of nodes
    v.sizes <- as.vector(table(clustering))
  } else {
    v.sizes <- tapply(V(the.graph)$size, clustering, sum)
  }
  
  for (neuron in nonempty.neurons) {
    v.neuron <- as.vector(V(the.graph)[which(clustering==neuron)])
    for (neuron2 in setdiff(nonempty.neurons, 1:neuron)) {
      v.neuron2 <- as.vector(V(the.graph)[which(clustering==neuron2)])
      if (is.null(E(the.graph)$weight)) {
        nb.edges <- length(E(the.graph)[v.neuron %--% v.neuron2])
      } else {
        nb.edges <- sum(E(the.graph)[v.neuron %--% v.neuron2]$weight)
      }
      if (nb.edges > 0) {
        p.edges <- c(p.edges, neuron, neuron2)
        p.edges.weights <- c(p.edges.weights, nb.edges)
      }
    }
  }
  proj.graph <- 
    graph_from_data_frame(matrix(p.edges, ncol=2, byrow=TRUE), directed=FALSE, 
                          vertices=data.frame("name"=nonempty.neurons,
                                              "size"=v.sizes))
  E(proj.graph)$weight <- p.edges.weights
  proj.graph <- set_graph_attr(proj.graph, "layout",
                               coord.clustering[nonempty.neurons,])
  return(proj.graph)
}

## S3 methods for somRes class objects
################################################################################

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
      c(round(summary(aov(object$data[,ind]~as.factor(object$clustering)))
              [[1]][1,4],digits=3),
        round(summary(aov(object$data[,ind]~as.factor(object$clustering)))
              [[1]][1,5],digits=8))
    })))
    names(res.anova) <- c("F", "pvalue")
    res.anova$significativity <- rep("",ncol(object$data))
    res.anova$significativity[res.anova$"pvalue"<0.05] <- "*"
    res.anova$significativity[res.anova$"pvalue"<0.01] <- "**"
    res.anova$significativity[res.anova$"pvalue"<0.001] <- "***"
    rownames(res.anova) <- colnames(object$data)
    
    cat("\n        Degrees of freedom : ", 
        summary(aov(object$data[,1]~as.factor(object$clustering)))[[1]][1,1],
        "\n\n")
    print(res.anova)  
    cat("\n")
  } else if (object$parameters$type=="korresp") {
    chisq.res <- chisq.test(object$data)
    if (chisq.res$p.value<0.05) sig <- "*"
    if (chisq.res$p.value<0.01) sig <- "**"
    if (chisq.res$p.value<0.001) sig <- "***"
    cat("\n     ", chisq.res$method, ":\n\n")
    cat("         X-squared               : ", chisq.res$statistic, "\n")
    cat("         Degrees of freedom      : ", chisq.res$parameter, "\n")
    cat("         p-value                 : ", chisq.res$p.value, "\n")
    cat("                 significativity : ", sig, "\n")
  } else if (object$parameters$type=="relational") {
    if (object$parameters$scaling=="cosine") {
      norm.data <- preprocessData(object$data, object$parameters$scaling)
    } else norm.data <- object$data
    sse.total <- sum(norm.data)/(2*nrow(norm.data))
    
    sse.within <- sum(sapply(unique(object$clustering), function(clust)
      sum(norm.data[object$clustering==clust,object$clustering==clust])/
        (2*sum(object$clustering==clust))))
    
    n.clusters <- length(unique(object$clustering))
    F.stat <- ((sse.total-sse.within)/sse.within) * 
      ((nrow(norm.data)-n.clusters)/(n.clusters-1))
    
    p.value <- 1-pf(F.stat, n.clusters-1, nrow(norm.data)-n.clusters)
    if (p.value<0.001) {
      sig <- "***"
    } else if (p.value<0.1) {
      sig <- "**"
    } else if (p.value<0.05) sig <- "*"
    
    cat("\n      ANOVA            : \n")
    cat("         F                       : ", F.stat, "\n")
    cat("         Degrees of freedom      : ", n.clusters-1, "\n")
    cat("         p-value                 : ", p.value, "\n")
    cat("                 significativity : ", sig, "\n")
  } 
}

predictRSOM <- function(object, x.new=NULL, radius=0, tolerance=10^(-10), 
                        A=NULL, B=NULL) {
  if (is.null(A) || is.null(B)) {
    winners <- predict.somRes(object, x.new=x.new, radius=radius,
                              tolerance=tolerance)
  } else {
    # distance between all prototypes and all data
    all.dist <- sweep(t(B), 1, -0.5*A, "+")
    # affectation to the closest prototype
    if (object$parameters$affectation=="standard") {
      winners <- apply(all.dist, 2, which.min)
    } else {
      # Heskes's soft affectation
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
          w.dist <- matrix(unlist(w.dist), nrow=25, byrow=TRUE)
        }
      }
      winners <- apply(w.dist, 2, which.min)
    }
  }
  return(winners)
}

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
#' \code{affectation="heskes"}, see \code{\link{initSOM}} for further details
#' about Heskes's soft affectation). Default value is \option{0}, 
#' which corresponds to a hard affectation.
#' @param tolerance numeric tolerance (to avoid numeric instability during 
#' 'cosine' pre-processing). Default value is \option{10^(-10)}
#' 
#' @details The number of columns of the new observations (or its length if only 
#' one observation is provided) must match the number of colums of the data set
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
#' Fabrice Rossi \email{fabrice.rossi@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{trainSOM}}
#' 
#' @examples 
#' set.seed(2343)
#' my.som <- trainSOM(x.data=iris[-100,1:4], dimension=c(5,5))
#' predict(my.som, iris[100,1:4])

predict.somRes <- function(object, x.new=NULL, ..., radius=0, 
                           tolerance=10^(-10)) {
  ## korresp
  if(object$parameters$type=="korresp") {
    if (!is.null(x.new)) 
      warning("For 'korresp' SOM, predict.somRes function can only be called on
              the original data set\n'object'. x.new not used!", 
              call.=TRUE)
    norm.x.data <- korrespPreprocess(object$data)
    
    winners.rows <- obsAffectation(norm.x.data[1:nrow(object$data),
                                               1:ncol(object$data)],
                                   object$prototypes[,1:ncol(object$data)],
                                   type=object$parameters$type,
                                   affectation=object$parameters$affectation,
                                   radius.type=object$parameters$radius.type, 
                                   radius=radius, 
                                   the.grid=object$parameters$the.grid)
    
    winners.cols <- 
      obsAffectation(norm.x.data[(nrow(object$data)+1):ncol(norm.x.data),
                                 (ncol(object$data)+1):ncol(norm.x.data)],
                     object$prototypes[,(ncol(object$data)+1):
                                         ncol(norm.x.data)],
                     type=object$parameters$type,
                     affectation=object$parameters$affectation,
                     radius.type=object$parameters$radius.type, 
                     radius=radius, 
                     the.grid=object$parameters$the.grid)
    
    winners <- c(winners.cols, winners.rows)
  } else if (object$parameters$type=="numeric") { ## numeric
    if (is.null(x.new)) {
      x.new <- object$data
    } else {
      if (is.null(dim(x.new)))
        x.new <- matrix(x.new, nrow=1, dimnames=list(1,colnames(object$data)))
      if (!is.matrix(x.new)) x.new <- as.matrix(x.new)
      # check data dimension
      if (ncol(x.new)!=ncol(object$data))
        stop("Number of columns of x.new does not correspond to number of 
             columns of the original data")
    }
    norm.x.new <- switch(object$parameters$scaling,
                         "unitvar"=scale(x.new,
                                         center=apply(object$data,2,mean),
                                         scale=apply(object$data,2,sd)),
                         "center"=scale(x.new,
                                        center=apply(object$data,2,mean),
                                        scale=FALSE),
                         "none"=x.new)
    norm.proto <- preprocessProto(object$prototypes, object$parameters$scaling,
                                  object$data)
    winners <- obsAffectation(norm.x.new, prototypes=norm.proto,
                              type=object$parameters$type,
                              affectation=object$parameters$affectation,
                              radius.type=object$parameters$radius.type,
                              radius=radius, 
                              the.grid=object$parameters$the.grid)
    
    } else if (object$parameters$type=="relational") { ## relational
      if (object$parameters$scaling == "cosine") 
        stop("'predict' is not implemented for cosine datasets")
      if (is.null(x.new)) {
        x.new <- object$data
      } else {
        if (is.null(dim(x.new)))
          x.new <- matrix(x.new, nrow=1, dimnames=list(1,colnames(object$data)))
        if (!is.matrix(x.new)) x.new <- as.matrix(x.new)
        # check data dimension
        if (ncol(x.new)!=ncol(object$data))
          stop("Number of columns of x.new does not correspond to number of 
               columns of the original data")
      }
      winners <- obsAffectation(x.new, prototypes=object$prototypes,
                                type=object$parameters$type, x.data=object$data,
                                affectation=object$parameters$affectation,
                                radius.type=object$parameters$radius.type,
                                radius=radius, 
                                the.grid=object$parameters$the.grid)
        
    }
  return(winners)
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
#' @param mode Specifies which distances should be computed.
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
#' most (\emph{i.e.}, two neurons are neighbors if they have a distance of type 
#' 'maximum' which is equal to 1).
#' 
#' @author Madalina Olteanu \email{madalina.olteanu@univ-paris1.fr}\cr
#' Nathalie Vialaneix \email{nathalie.vialaneix@inrae.fr}
#' 
#' @seealso \code{\link{trainSOM}}
#' 
#' @examples
#' set.seed(2343)
#' my.som <- trainSOM(x.data=iris[,1:4], dimension=c(5,5))
#' protoDist(my.som)

protoDist <- function(object, mode,...) {
  UseMethod("protoDist")
}

#' @export

protoDist.somRes <- function(object, mode=c("complete","neighbors"), ...) {
  mode <- match.arg(mode)
  complete <- (mode=="complete")
  norm.proto <- preprocessProto(object$prototypes, object$parameters$scaling, 
                                object$data)
  if (object$parameters$type=="relational") {
    x.data <- preprocessData(object$data, object$parameters$scaling)
  } else x.data <- NULL
  
  distances <- calculateProtoDist(norm.proto, object$parameters$the.grid,
                                  object$parameters$type, complete, x.data)
  return(distances)
}

projectIGraph.somRes <- function(object, init.graph, ...) {
  if (!is_igraph(init.graph)) {
    stop("'init.graph' must be an igraph object\n", call.=TRUE)
  }
  if (gorder(init.graph)!=length(object$clustering)) {
    stop("The number of vertexes of 'init.graph' does not match the clustering length\n", call.=TRUE)
  }
  proj.graph <- projectGraph(init.graph, object$clustering,
                             object$parameters$the.grid$coord)
  return(proj.graph)
}

projectIGraph <- function(object, init.graph, ...) {
  UseMethod("projectIGraph")
}