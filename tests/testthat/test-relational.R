context("Test that outputs of relational SOM are convex combinations")

iris.dist <- dist(iris[1:30,1:4], method="minkowski", diag=TRUE, upper=TRUE, 
                  p=4)
set.seed(1804)
rsom <- trainSOM(x.data=iris.dist, type="relational")

test_that("Test that all prototypes sum to 1", {
  expect_equal(unname(rowSums(rsom$prototypes)), 
               rep(1, prod(rsom$parameters$the.grid$dim)))
})

test_that("Test that all prototypes have non negative coefficients", {
  expect_equal(sum(rsom$prototypes < 0), 0)
})