#library(testthat)

test_that("systemPackages() works", {

  f <- kwb.package::systemPackages

  result <- f()

  expect_type(result, "character")
  expect_true("base" %in% result)
  expect_true(all(result %in% rownames(installed.packages())))
  
})
