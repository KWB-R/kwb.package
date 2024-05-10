#library(testthat)
#source("tests/testthat/setup.R")

test_that("getPackageLicences() works", {

  f <- kwb.package::getPackageLicences

  expect_error(f())

  # CRAN_PACKAGE_DB is loaded in testthat/setup.R
  result <- f("kwb.utils", db = CRAN_PACKAGE_DB)
  
  expect_true(nrow(result) == 1L)
  expect_true(all(is.na(kwb.utils::removeColumns(result, c("package", "licence")))))
  expect_identical(result$licence, "<not_found>")
  
  expect_identical(f("dplyr")$package, "dplyr")
})
