#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-05-02 17:58:25.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("compareInstalledVersions() works", {

  f <- kwb.package::compareInstalledVersions

  expect_error(
    f()
    # Argument "lib1" fehlt (ohne Standardwert)
  )

})
