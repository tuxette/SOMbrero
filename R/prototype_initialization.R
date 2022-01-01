# Prototype initialization ####################################################

initProto <- function(parameters, norm.x.data) {
  dim1 <- parameters$the.grid$dim[1]
  dim2 <- parameters$the.grid$dim[2]
  type <- parameters$type
  nb.proto <- prod(c(dim1, dim2))
  
  prototypes <- switch(parameters$init.proto,
                       "random" = randomPrototypes(nb.proto, type, norm.x.data),
                       "obs"    = obsPrototypes(nb.proto, type, norm.x.data),
                       "pca"    = pcaPrototypes(dim1, dim2, type, norm.x.data))
  
  if (!is.matrix(prototypes)) prototypes <- as.matrix(prototypes)

  return(prototypes)
}


## Random prototypes ####

randomPrototypes <- function(nb.proto, type, norm.x.data) {
  if (type == "relational") {
    # in [0,1] and sum to 1
    matrix.size <- prod(nb.proto, nrow(norm.x.data))
    prototypes <- runif(matrix.size)
    prototypes <- matrix(prototypes, nrow = nb.proto)
    prototypes <- prototypes / rowSums(prototypes)
  
    } else {
    
    # both numeric and korresp: in the range of normalized data
    min.val <- apply(norm.x.data, 2, min, na.rm = TRUE)
    max.val <- apply(norm.x.data, 2, max, na.rm = TRUE)
    prototypes <- sapply(1:ncol(norm.x.data), function(ind) {
      runif(nb.proto, min.val[ind], max.val[ind])
    })
  }
  
  return(prototypes)
}


## Observation prototypes ####

obsPrototypes <- function(nb.proto, type, norm.x.data) {
  if (any(is.na(norm.x.data))) norm.x.data <- na.omit(norm.x.data)
  if (nrow(norm.x.data) < nb.proto) 
    stop("Not enough complete observations for 'obs' initialization.")
  
  # selected observations
  selected.obs <- sample(1:nrow(norm.x.data), nb.proto, replace = TRUE)
  
  if (type %in% c("korresp", "numeric")) {
    # directly select from normalized data
    prototypes <- norm.x.data[selected.obs, ]
    
    # replace columns with missing values by average value of the column
    prototypes <- mean.imputation(prototypes)
    
  } else if (type == "relational") {
    # selected observations have a coefficients equal to 1
    prototypes <- matrix(0, nrow = nb.proto, ncol = nrow(norm.x.data))
    prototypes[cbind(1:nrow(prototypes), selected.obs)] <- 1
  }
  
  return(prototypes)
}


## PCA / MDS based prototypes ####

pcaPrototypes <- function(dim1, dim2, type, norm.x.data) {
  
  if (dim1 >= dim2) {
    x.ev <- 1
    y.ev <- 2
  } else {
    x.ev <- 2
    y.ev <- 1
  }
  
  if (type == "numeric") {
    # perform PCA (with mean imputation in case of NA)
    norm.x.data <- mean.imputation(norm.x.data)
    
    data.pca <- princomp(norm.x.data)
    scores <- data.pca$scores[, c(x.ev, y.ev)]
  } else { # type is 'relational'
    # perform MDS
    data.mds <- cmdscale(norm.x.data)
    scores <- data.pca$scores[, c(x.ev, y.ev)]
  }
  
  x <- seq(from       = quantile(scores[, 1], .025), 
           to         = quantile(scores[, 1], .975),
           length.out = dim1)
  y <- seq(from       = quantile(scores[, 2], .025), 
           to         = quantile(scores[, 2], .975),
           length.out = dim2)
  base <- as.matrix(expand.grid(x = x, y = y))
  
  # search for the closest observation
  scores.vs.base <- matrix.dist(base, scores)^2
  closest.obs <- apply(scores.vs.base, 1, which.min)
  
  if (type == "numeric") {
    # prototype is one of the observation
    prototypes <- norm.x.data[closest.obs, ]
  } else { # type is 'relational'
    # prototypes are coefficients: the closest only is equal to 1
    prototypes <- matrix(0, ncol = nrow(norm.x.data), nrow = dim1 * dim2)
    prototypes[closest.obs] <- 1
  }
  
  return(prototypes)
}