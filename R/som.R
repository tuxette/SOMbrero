##### SOM algorithm functions
################################################################################

##### Auxiliary functions
################################################################################

calculateRadius <- function(the.grid, radius.type, ind.t, maxit) {
  ## TODO: implement other radius types
  # ind.t: iteration index
  if (radius.type=="letremy") {
    r0 <- max(c(floor(the.grid$dim[1]/2), floor(the.grid$dim[2]/2)))
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
  }
  r
}

selectNei <- function(the.neuron, the.grid, radius) {
  if (the.grid$dist.type=="letremy") {
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
                               method=the.grid$dist.type))[the.neuron,]
    the.nei <- which(the.dist<=radius)
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
    all.nei <- sapply(1:prod(the.grid$dim),selectNei,the.grid=the.grid,radius=1)
    all.nei <- sapply(1:prod(the.grid$dim), function(neuron) 
      setdiff(all.nei[[neuron]],neuron))
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
    cont.table/outer(rowSums(cont.table), 
                     sqrt(colSums(cont.table)/sum(cont.table)))  
  # column profiles
  both.profiles[(nrow(cont.table)+1):(nrow(cont.table)+ncol(cont.table)),
                (ncol(cont.table)+1):(ncol(cont.table)+nrow(cont.table))] <- 
    t(cont.table)/outer(colSums(cont.table), 
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
    }
  } else {
    prototypes <- switch(parameters$scaling,
                         "unitvar"=scale(parameters$proto0, 
                                         center=apply(x.data,2,mean),
                                         scale=apply(x.data,2,sd)),
                         "center"=scale(parameters$proto0, 
                                        center=apply(x.data,2,mean),
                                        scale=FALSE),
                         "none"=as.matrix(parameters$proto0),
                         "chi2"=as.matrix(parameters$proto0))
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
oneObsAffectation <- function(x.new, prototypes, type, x.data=NULL) {
  if (type=="relational") {
    the.neuron <- which.min(prototypes%*%x.new-
                              0.5*diag(prototypes%*%x.data%*%
                                         t(prototypes)))
  } else the.neuron <- which.min(apply(prototypes, 1, distEuclidean, y=x.new))

  the.neuron
}

# Step 7: Update of prototypes
prototypeUpdate <- function(type, the.nei, epsilon, prototypes, rand.ind,
                            sel.obs) {
  if (type=="relational") {
    indic <- matrix(0,nrow=length(the.nei),ncol=ncol(prototypes)) 
    indic[,rand.ind] <- 1
    prototypes[the.nei,] <- (1-epsilon)*prototypes[the.nei,] + epsilon*indic    
  } else {
    prototypes[the.nei,] <- (1-epsilon)*prototypes[the.nei,] +
      epsilon*outer(rep(1,length(the.nei)), sel.obs)
  }
}

# Step 8: calculate intermediate energy
# TODO: It would probably be better to implement a function 'distEltProto'
calculateClusterEnergy <- function(cluster, x.data, clustering, prototypes,
                                   parameters, radius) {
  if (parameters$type=="numeric" || parameters$type=="korresp") {
    if (parameters$radius.type=="letremy") {
      the.nei <- selectNei(cluster, parameters$the.grid, radius)
      if (sum(clustering%in%the.nei)>0) {
        return(sum((x.data[which(clustering%in%the.nei),]-
                      outer(rep(1,sum(clustering%in%the.nei)),
                                 prototypes[cluster,]))^2))
      }
    }
  } else if (parameters$type=="relational") {
    if (parameters$radius.type=="letremy") {
      the.nei <- selectNei(cluster, parameters$the.grid, radius)
      if (sum(clustering%in%the.nei)>0) {
        return(sum(prototypes%*%x.data[,which(clustering%in%the.nei)]-0.5*
                     diag(prototypes%*%x.data%*%t(prototypes))))
      }      
    }
  }
}

calculateEnergy <- function(x.data, clustering, prototypes, parameters, ind.t) {
  if (parameters$type=="numeric" || parameters$type=="korresp") {
    if (parameters$radius.type=="letremy") {
      radius <- calculateRadius(parameters$the.grid, parameters$radius.type,
                                ind.t, parameters$maxit)
      return(sum(unlist(sapply(1:nrow(prototypes), calculateClusterEnergy,
                               x.data=x.data, clustering=clustering, 
                               prototypes=prototypes, parameters=parameters,
                               radius=radius)))/
               nrow(x.data)/nrow(prototypes))
    }
  } else if (parameters$type=="relational") {
    if (parameters$radius.type=="letremy") {
      radius <- calculateRadius(parameters$the.grid, parameters$radius.type,
                              ind.t, parameters$maxit)
      return(sum(unlist(sapply(1:nrow(prototypes), calculateClusterEnergy,
                             x.data=x.data, clustering=clustering, 
                             prototypes=prototypes, parameters=parameters,
                             radius=radius)))/
             nrow(x.data)/nrow(prototypes))
    }
  }
}

##### Main function
################################################################################
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
        (!(identical(x.data, t(x.data))) || (sum(diag(x.data)!=0)>0)))
    stop("data do not match chosen SOM type ('relational')\n", call.=TRUE)
  
  # Initialize parameters and print
  parameters <- do.call("initSOM", param.args)
  if (parameters$verbose) {
    cat("Self-Organizing Map algorithm...\n")
    print.paramSOM(parameters)
  }
  
  # Check proto0 also now that the parameters have been initialized
  if (!is.null(param.args$proto0)) {
    if ((param.args$type=="korresp")&&
          (!identical(dim(param.args$proto0),
                      as.integer(c(prod(param.args$dimension),
                                   ncol(x.data)+nrow(x.data)))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
         in the current SOM, prototypes must have ", 
           prod(param.args$dimension), " rows and ", 
           ncol(x.data)+nrow(x.data), " columns\n", call.=TRUE)
    } else if (!identical(dim(param.args$proto0),
                          as.integer(c(prod(param.args$dimension),
                                       ncol(x.data))))) {
      stop("initial prototypes dimensions do not match SOM parameters:
         in the current SOM, prototypes must have ", 
           prod(param.args$dimension), " rows and ", 
           ncol(x.data), " columns\n", call.=TRUE)
    }
  }
  
  ## Step 2: Preprocess the data
  # Scaling
  norm.x.data <- switch(parameters$scaling,
                        "unitvar"=scale(x.data, center=TRUE, scale=TRUE),
                        "center"=scale(x.data, center=TRUE, scale=FALSE),
                        "none"=as.matrix(x.data),
                        "chi2"=korrespPreprocess(x.data))
  
  ## Step 3: Initialize prototypes
  prototypes <- initProto(parameters, norm.x.data, x.data)
  
  # Step 4: Iitialize backup if needed
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
    
    ## Step 5: Randomly choose an observation
    rand.ind <- selectObs(ind.t, dim(x.data), parameters$type)
    sel.obs <- norm.x.data[rand.ind,]
    
    ## Step 6: Assignment step
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
    # Assign
    winner <- oneObsAffectation(cur.obs, cur.prototypes, parameters$type,
                                norm.x.data)
    
    ## Step 7: Representation step
    # Radius value
    radius <- calculateRadius(parameters$the.grid, parameters$radius.type,
                              ind.t, parameters$maxit)
    the.nei <- selectNei(winner, parameters$the.grid, radius)
    # TODO: scale epsilon with a parameter???
    epsilon <- 0.3/(1+0.2*ind.t/prod(parameters$the.grid$dim))
    # Update
    prototypes[the.nei,] <- prototypeUpdate(parameters$type, the.nei, epsilon, 
                                            prototypes, rand.ind, sel.obs)
    
    ## Step 8: Intermediate backups (if needed)
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
                            "chi2"=prototypes)
        colnames(out.proto) <- colnames(norm.x.data)
        rownames(out.proto) <- 1:prod(parameters$the.grid$dim)
        res <- list("parameters"=parameters, "prototypes"=out.proto, 
                    "data"=x.data)
        class(res) <- "somRes"
        
        ind.s <- match(ind.t,backup$steps)
        backup$prototypes[[ind.s]] <- out.proto
        backup$clustering[,ind.s] <- predict.somRes(res, x.data)
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
                          "chi2"=prototypes)
      
      res <- list("parameters"=parameters, "prototypes"=out.proto,
                  "data"=x.data)
      class(res) <- "somRes"
      clustering <- predict.somRes(res, x.data)
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


## S3 methods for somRes class objects
################################################################################

print.somRes <- function(x, ...) {
  cat("      Self-Organizing Map object...\n")
  cat("        ", x$parameters$mode, "learning, type:", x$parameters$type,"\n")
  cat("        ", x$parameters$the.grid$dim[1],"x",
      x$parameters$the.grid$dim[2],
      "grid with",x$parameters$the.grid$topo, "topology\n")
}

summary.somRes <- function(object, ...) {
  cat("\nSummary\n\n")
  cat("      Class : ", class(object),"\n\n")
  print(object)
  cat("\n      Final energy:", object$energy,"\n")
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
  }
}

predict.somRes <- function(object, x.new, ...) {
  if (is.null(dim(x.new))) x.new <- matrix(x.new,nrow=1,
                                           dimnames=list(1,
                                                         colnames(object$data)))
  if(object$parameters$type!="korresp") {
    norm.x.new <- switch(object$parameters$scaling,
                         "unitvar"=scale(x.new,
                                         center=apply(object$data,2,mean),
                                         scale=apply(object$data,2,sd)),
                         "center"=scale(x.new,
                                        center=apply(object$data,2,mean),
                                        scale=FALSE),
                         "none"=as.matrix(x.new))
    norm.proto <- switch(object$parameters$scaling,
                         "unitvar"=scale(object$prototypes, 
                                         center=apply(object$data,2,mean),
                                         scale=apply(object$data,2,sd)),
                         "center"=scale(object$prototypes, 
                                        center=apply(object$data,2,mean),
                                        scale=FALSE),
                         "none"=object$prototypes)
    winners <- apply(norm.x.new, 1, oneObsAffectation,
                     prototypes=norm.proto, type=object$parameters$type,
                     x.data=object$data)
  } else {
    if (!identical(as.matrix(x.new), object$data))
      warning("For 'korresp' SOM, predict.somRes function can only be called on
              the original data set\n'object' replaced", 
              call.=TRUE)
    norm.x.data <- korrespPreprocess(object$data)
    winners.rows <- apply(norm.x.data[1:nrow(object$data),1:ncol(object$data)],
                          1, oneObsAffectation,
                          prototypes=object$prototypes[,1:ncol(object$data)],
                          type=object$parameters$type)
    winners.cols <- apply(norm.x.data[(nrow(object$data)+1):ncol(norm.x.data),
                                      (ncol(object$data)+1):ncol(norm.x.data)],
                          1, oneObsAffectation,
                          prototypes=object$prototypes[,(ncol(object$data)+1):
                                                         ncol(norm.x.data)],
                          type=object$parameters$type)
    winners <- c(winners.cols,winners.rows)
  }
  winners
}
  
protoDist.somRes <- function(object, mode=c("complete","neighbors"), ...) {
  mode <- match.arg(mode)
  complete <- (mode=="complete")
  if (object$parameters$type=="relational") {
    x.data <- object$data
  } else x.data <- NULL

  distances <- calculateProtoDist(object$prototypes, object$parameters$the.grid,
                                  object$parameters$type, complete, x.data)
  
  return(distances)
}

protoDist <- function(object, mode,...) {
  UseMethod("protoDist")
}