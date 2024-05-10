#library(testthat)

test_that("downloadGitHubPackage() works", {

  f <- kwb.package::downloadGitHubPackage

  expect_error(f())

  suppressMessages(result <- f("kwb-r/kwb.utils"))
  
  expect_true(endsWith(result, ".tar.gz"))
  expect_true(file.exists(result))
  
})
