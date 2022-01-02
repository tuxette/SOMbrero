context("Check that 'predict.somRes' gives identical prediction to the final
        clustering for training data.")

data("lesmis")
data("presidentielles2002")
set.seed(1404)
removed <- cbind(sample(1:150, 20, replace = TRUE), 
                 sample(1:4, 20, replace = TRUE))
x.data <- iris[, 1:4]
x.data[removed] <- NA

test_that("case numeric - scaling 'none'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, scaling = "none")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric - scaling 'center'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, scaling = "center")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric - scaling 'unitvar'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, scaling = "unitvar")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric - radius 'letremy'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, radius.type = "letremy")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric - affectation 'heskes'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, affectation = "heskes")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric - radius 'letremy' and affectation 'heskes'", {
  set.seed(1222)
  nsom <- trainSOM(iris[1:30, 1:4], maxit = 10, affectation = "heskes", 
                   radius.type = "letremy")
  expect_equal(predict(nsom, iris[1:30, 1:4]), nsom$clustering)
  expect_equal(unname(predict(nsom, iris[1, 1:4])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - scaling 'none'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, scaling = "none")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - scaling 'center'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, scaling = "center")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - scaling 'unitvar'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, scaling = "unitvar")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - radius 'letremy'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, radius.type = "letremy")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - affectation 'heskes'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, affectation = "heskes")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case numeric with missing entries - radius 'letremy' and affectation 'heskes'", {
  set.seed(1222)
  nsom <- trainSOM(x.data[1:30, ], maxit = 10, affectation = "heskes", 
                   radius.type = "letremy")
  expect_equal(predict(nsom, x.data[1:30, ]), nsom$clustering)
  expect_equal(unname(predict(nsom, x.data[1, ])), unname(nsom$clustering[1]))
  expect_equal(predict(nsom), nsom$clustering)
})

test_that("case relational - default options", {
  set.seed(1222)
  rsom <- trainSOM(dissim.lesmis, type = "relational", maxit = 10, 
                   scaling =  "none")
  expect_equal(predict(rsom, dissim.lesmis), rsom$clustering)
  expect_equal(unname(predict(rsom, dissim.lesmis[1, ])), unname(rsom$clustering[1]))
  expect_equal(predict(rsom), rsom$clustering)
})

test_that("case relational - radius 'letremy'", {
  set.seed(1222)
  rsom <- trainSOM(dissim.lesmis, type = "relational", maxit = 10, 
                   radius.type = "letremy")
  expect_equal(predict(rsom, dissim.lesmis), rsom$clustering)
  expect_equal(unname(predict(rsom, dissim.lesmis[1, ])), unname(rsom$clustering[1]))
  expect_equal(predict(rsom), rsom$clustering)
})

test_that("case relational - affectation 'heskes'", {
  set.seed(1222)
  rsom <- trainSOM(dissim.lesmis, type = "relational", maxit = 10, 
                   affectation = "heskes")
  expect_equal(predict(rsom, dissim.lesmis), rsom$clustering)
  expect_equal(unname(predict(rsom, dissim.lesmis[1, ])), unname(rsom$clustering[1]))
  expect_equal(predict(rsom), rsom$clustering)
})

test_that("case relational - radius 'letremy' and affectation 'heskes'", {
  set.seed(1222)
  rsom <- trainSOM(dissim.lesmis, type = "relational", maxit = 10, 
                   affectation = "heskes", radius.type = "letremy")
  expect_equal(predict(rsom, dissim.lesmis), rsom$clustering)
  expect_equal(unname(predict(rsom, dissim.lesmis[1, ])), unname(rsom$clustering[1]))
  expect_equal(predict(rsom), rsom$clustering)
})

test_that("case korresp", {
  set.seed(1222)
  korr <- trainSOM(presidentielles2002, type = "korresp", maxit =  10)
  expect_equal(predict(korr), korr$clustering)
})

