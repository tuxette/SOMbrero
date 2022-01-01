# Various preprocessings for data #############################################

## Data preprocessing ####

preprocessData <- function(x.data, scaling, tolerance = 10^(-10)) {
  ## note: NA in 'x.data' seems to be properly handled for 'unitvar' and 'center'
  switch(scaling,
         "unitvar" = unitvarPreprocess(x.data),
         "center"  = centerPreprocessing(x.data),
         "none"    = as.matrix(x.data),
         "chi2"    = korrespPreprocess(x.data),
         "cosine"  = cosinePreprocess(x.data, tolerance = tolerance))
}


### 'unitvar' preprocessing

unitvarPreprocess <- function(x.data) {
  all.variances <- apply(x.data, 2, sd, na.rm = TRUE)
  if (any(all.variances == 0)) {
    ### remove data with null variance
    to.remove <- which(all.variances == 0)
    name.remove <- paste0(colnames(x.data)[to.remove], collapse = ", ")
    out.message <- "Removing constant variable(s) (required by 'unit variance' scaling): columns"
    out.message <- paste(out.message, name.remove)
    warning(out.message, call. = TRUE, immediate. = TRUE)
    x.data <- x.data[, -to.remove]
  }

  return(scale(x.data, center = TRUE, scale = TRUE))
}


### 'center' preprocessing

centerPreprocessing <- function(x.data) {
  return(scale(x.data, center = TRUE, scale = FALSE))
}


### 'cosine' preprocessing (for "relational" case)

### TODO: code cleaning is needed
cosinePreprocess <- function(diss.matrix, x.new = NULL, tolerance = 10^(-10)) {
  ## TODO: remove x.new that is not used anymore
  sum.posdata <- rowSums(abs(diss.matrix))
  if (any(sum.posdata == 0)) {
    stop("One of the rows is 0. Cosine preprocessing can not be performed.",
         call. = TRUE, immediate = TRUE)
  }
  
  # tolerance is used to solve numeric instabilities
  # similarity matrix by double centering
  sim.matrix <- - 0.5* (diag(1, nrow(diss.matrix)) - 1/nrow(diss.matrix)) %*%
    diss.matrix %*% (diag(1, nrow(diss.matrix)) - 1/nrow(diss.matrix))
  sim.matrix <- round(sim.matrix, - log10(tolerance))
  
  if (is.null(x.new)) {
    # normalize the original dissimilarity matrix
    scaled.ker <- sweep(sweep(sim.matrix, 2, sqrt(diag(sim.matrix)), "/"),
                        1, sqrt(diag(sim.matrix)), "/")
    # came back to dissimilarity
    # if cosine preprocess is useless it is the original dissimilarity
    scaled.diss <- 2 - 2 * scaled.ker
    rownames(scaled.diss) <- rownames(diss.matrix)
    colnames(scaled.diss) <- colnames(diss.matrix)
    scaled.diss <- round(scaled.diss, - log10(tolerance))
  } else {
    # normalize additional dissimilarity lines like the similarity matrix
    sim.x <- t(apply(x.new, 1, function(x) 
      - .5 * (x - mean(x) - colMeans(diss.matrix) + mean(diss.matrix))))
    sim.x <- round(sim.x, -log10(tolerance))
    auto.sim <- round(apply(x.new,1,mean) - mean(diss.matrix) / 2, 
                      -log10(tolerance))
    if (nrow(x.new) > 1) {
      scaled.ker <- sweep(sweep(sim.x, 2, sqrt(diag(sim.matrix)), "/"), 1,
                          sqrt(auto.sim), "/")
    } else {
      scaled.ker <- sweep(sim.x, 2, sqrt(diag(sim.matrix)), "/") / sqrt(auto.sim)
    }
    scaled.diss <- 2 - 2 * scaled.ker
    colnames(scaled.diss) <- colnames(diss.matrix)
    rownames(scaled.diss) <- rownames(x.new)
    scaled.diss <- round(scaled.diss, -log10(tolerance))
  }
  
  return(scaled.diss)
}


### 'korresp' preprocessing (for "relational" case)

### TODO: code cleaning is needed
korrespPreprocess <- function(cont.table) {
  both.profiles <- matrix(0, nrow = nrow(cont.table) + ncol(cont.table),
                          ncol = ncol(cont.table) + nrow(cont.table))
  # row profiles
  both.profiles[1:nrow(cont.table), 1:ncol(cont.table)] <-
    cont.table / 
    outer(sqrt(rowSums(cont.table)), sqrt(colSums(cont.table) / sum(cont.table)))  
  # column profiles
  both.profiles[(nrow(cont.table)+1):(nrow(cont.table) + ncol(cont.table)),
                (ncol(cont.table)+1):(ncol(cont.table) + nrow(cont.table))] <- 
    t(cont.table) / outer(sqrt(colSums(cont.table)), 
                          sqrt(rowSums(cont.table)/sum(cont.table)))
  # best column to complete row profiles
  best.col <- apply(both.profiles[1:nrow(cont.table), 1:ncol(cont.table)],
                    1, which.max)
  both.profiles[1:nrow(cont.table), (ncol(cont.table) + 1):ncol(both.profiles)] <- 
    both.profiles[best.col + nrow(cont.table),
                  (ncol(cont.table) + 1):ncol(both.profiles)]
  # best row to complete col profiles
  best.row <- apply(both.profiles[(nrow(cont.table)+1):
                                    (nrow(cont.table) + ncol(cont.table)),
                                  (ncol(cont.table)+1):
                                    (ncol(cont.table) + nrow(cont.table))],
                    1, which.max)
  both.profiles[(nrow(cont.table)+1):(nrow(cont.table) + ncol(cont.table)),
                1:ncol(cont.table)] <-
    both.profiles[best.row, 1:ncol(cont.table)]
  # names
  rownames(both.profiles) <- c(rownames(cont.table),colnames(cont.table))
  colnames(both.profiles) <- c(colnames(cont.table),rownames(cont.table))
  return(both.profiles)
}

## Prototype preprocessing ####

preprocessProto <- function(prototypes, scaling, x.data) {
  if (scaling %in% c("unitvar", "center")) 
    x.mean <- colMeans(x.data, na.rm = TRUE)
  
  if (scaling == "unitvar")
    x.var <- apply(x.data, 2, sd, na.rm = TRUE)
  
  switch(scaling,
         "unitvar" = scale(prototypes, center = x.mean, scale = x.var),
         "center"  = scale(prototypes, center = x.mean, scale = FALSE),
         "none"    = prototypes,
         "chi2"    = prototypes,
         "cosine"  = prototypes)
}

dePreprocessProto <- function(prototypes, scaling, x.data) {
  if (scaling %in% c("unitvar", "center")) 
    x.mean <- - colMeans(x.data, na.rm = TRUE)
  
  if (scaling == "unitvar") {
    x.var <- 1 / apply(x.data, 2, sd, na.rm = TRUE)
    x.mean <- x.mean * x.var
  }
  
  switch(scaling,
         "unitvar" = scale(prototypes, center = x.mean, scale = x.var),
         "center"  = scale(prototypes, center = x.mean, scale = FALSE),
         "none"    = prototypes,
         "chi2"    = prototypes,
         "cosine"  = prototypes)
}
