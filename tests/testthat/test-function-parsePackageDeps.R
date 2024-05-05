#library(testthat)

test_that("parsePackageDeps() works", {

  f <- kwb.package:::parsePackageDeps

  expect_error(f())

  description <- list(
    package = "abc", 
    imports = "a(>= 1.1.1), b(>= 2.1),c(>= 3)"
  )
  
  result <- f(description)

  expect_s3_class(result, "data.frame")  
  expect_identical(nrow(result), 3L)
  expect_identical(names(result), c("name", "compare", "version", "type"))
  expect_true(all(result$type == "imports"))
  
})
