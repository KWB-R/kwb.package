#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-05-02 18:06:07.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("installPackage() works", {

  f <- kwb.package:::installPackage

  expect_error(
    f()
    # Argument "packageName" fehlt (ohne Standardwert)
  )

})
