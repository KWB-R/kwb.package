#library(testthat)

test_that("cranVersions() works", {

  f <- kwb.package::cranVersions

  expect_error(f())

  result <- f("abc")
  
  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c(
    "package", "version", "date", "date_type", "package_source_url"
  ))
  
})
