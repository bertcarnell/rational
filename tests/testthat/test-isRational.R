testthat::context("test-isRational")

testthat::test_that("Test isRational",{
  expect_true(is.rationalS3(rational(1L, 2L, "S3")))
  expect_true(!is.rationalS3(7.1))
  expect_true(is.rationalR6(rational(1L, 2L, "R6")))
  expect_true(!is.rationalR6("no"))
  expect_true(is.rationalS4(rational(1L, 2L, "S4")))
  expect_true(!is.rationalS4(TRUE))
  expect_true(is.rational(rational(1L, 2L, "S3")))
  expect_true(!is.rationalS3(7.1))
  expect_true(is.rational(rational(1L, 2L, "R6")))
  expect_true(is.rational(rational(1L, 2L, "S4")))

  expect_true(is.rationalS3(rational(1L, 2L, "S3"), inherit = TRUE))
  expect_true(is.rationalR6(rational(1L, 2L, "R6"), inherit = TRUE))
  expect_true(is.rationalS4(rational(1L, 2L, "S4"), inherit = TRUE))
  expect_true(is.rational(rational(1L, 2L, "S4"), inherit = TRUE))
})
