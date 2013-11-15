# Check the equivalence between numeric SOM and 
# relational SOM on squared euclidian distance matrix

library(SOMbrero)

set.seed(123)
nsom <- trainSOM(iris[1:30, 1:4], init.proto= "obs", scaling= "none", 
                 maxit= 50)

set.seed(123)
iris.dist <- dist(iris[1:30, 1:4], method="euclidian", diag=TRUE, upper=TRUE)^2
rsom <- trainSOM(x.data=iris.dist, type="relational", scaling= "none", 
                 maxit= 50)

stopifnot(identical(nsom$clustering, rsom$clustering))