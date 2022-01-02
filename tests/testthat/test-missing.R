context("Check that plots are produced properly for data with missing entries.")

set.seed(1404)
removed <- cbind(sample(1:150, 20, replace = TRUE), 
                 sample(1:4, 20, replace = TRUE))
x.data <- iris[, 1:4]
x.data[removed] <- NA
nsom <- trainSOM(iris[, 1:4], maxit = 10)

test_that("All 'obs' plots are produced for inputs with missing entries", {
  expect_equal(class(plot(nsom)), c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "obs", type = "color", variable = 2)), 
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "obs", type = "lines")),
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "obs", type = "meanline")), 
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "obs", type = "barplot")), 
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "obs", type = "boxplot")), 
               c("gg", "ggplot"))
})

test_that("Energy plot is produced for inputs with missing entries", {
  nsome <- trainSOM(iris[, 1:4], maxit = 10, nb.save = 10)
  expect_equal(class(plot(nsome, what = "energy")), c("gg", "ggplot"))
})

test_that("All 'add' plots are produced for inputs with missing entries", {
  set.seed(201)
  removed <- sample(1:150, 10, replace = TRUE)
  add <- iris$Species
  add[removed] <- NA
  
  expect_equal(class(plot(nsom, what = "add", type = "pie", variable = add)),
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "add", type = "names", variable = add)),
                     c("gg", "ggplot"))
  
  add2 <- matrix(0, ncol = 3, nrow = 150)
  add2[iris$Species == "versicolor", 1] <- 1
  add2[iris$Species == "virginica", 2] <- 1
  add2[iris$Species == "setosa", 3] <- 1
  add2[removed, ] <- NA
  colnames(add2) <- c("versicolor", "virginica", "setosa")
  expect_equal(class(plot(nsom, what = "add", type = "words", variable = add2)),
               c("gg", "ggplot"))
  
  add3 <- x.data[, 1]
  expect_equal(class(plot(nsom, what = "add", type = "color", variable = add3)), 
               c("gg", "ggplot"))
  
  add4 <- x.data
  expect_error(plot(nsom, what = "add", type = "lines", variable = add4))
  expect_equal(class(plot(nsom, what = "add", type = "meanline", 
                          variable = add4)), 
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "add", type = "barplot", 
                          variable = add4)), 
               c("gg", "ggplot"))
  expect_equal(class(plot(nsom, what = "add", type = "boxplot", 
                          variable = add4)), 
               c("gg", "ggplot"))
})

test_that("Quality can be computed for inputs with missing entries", {
  out_quality <- quality(nsom)
  expect_true(out_quality$topographic >= 0 && out_quality$topographic <= 1)
  expect_true(is.numeric(out_quality$quantization))
})
