#library(testthat)

testthat::skip("Do not install a package during testing!")

test_that("installRemotes() works", {

  f <- kwb.package::installRemotes

  f()

})
