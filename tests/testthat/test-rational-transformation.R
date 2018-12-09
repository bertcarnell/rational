testthat::context("test-rational-transformation")

testthat::test_that("Test transformation S4", {
  expect_equal(c(2/5, 3), as.numeric(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_equal(c(0L, 3L), as.integer(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_equal(2L, as.integer(rational(8L, 3L, "S4")))
  expect_equal(c("2 / 5", "3 / 1"), as.character(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_true(rational(333L, 10L, "S4") == as.rationalS4(33.3))
})

testthat::test_that("Test transformation S3", {
  expect_true(rational(333L, 10L, "S3") == as.rationalS3(33.3))
  expect_true(all(rational(c(333L, 33L), c(10L, 10L), "S3") == as.rationalS3(c(33.3, 3.3))))
  expect_equal(1/3, as.double(rational(1L, 3L, "S3")))
  expect_equal(c(1/3, 2/5), as.double(rational(c(1L, 2L), c(3L, 5L), "S3")))
  expect_equal(1/3, as.numeric(rational(1L, 3L, "S3")))
  expect_equal(c(1/3, 2/5), as.numeric(rational(c(1L, 2L), c(3L, 5L), "S3")))
  expect_equal(0, as.integer(rational(1L, 3L, "S3")))
  expect_equal(c(0,0), as.integer(rational(c(1L, 2L), c(3L, 5L), "S3")))
})

testthat::test_that("Test transformation R6", {
  expect_true(rational(333L, 10L, "R6") == as.rationalR6(33.3))
  expect_true(all(rational(c(333L, 33L), c(10L, 10L), "R6") == as.rationalR6(c(33.3, 3.3))))
  expect_equal(1/3, as.double(rational(1L, 3L, "R6")))
  expect_equal(c(1/3, 2/5), as.double(rational(c(1L, 2L), c(3L, 5L), "R6")))
  expect_equal(1/3, as.numeric(rational(1L, 3L, "R6")))
  expect_equal(c(1/3, 2/5), as.numeric(rational(c(1L, 2L), c(3L, 5L), "R6")))
  expect_equal(0, as.integer(rational(1L, 3L, "R6")))
  expect_equal(c(0,0), as.integer(rational(c(1L, 2L), c(3L, 5L), "R6")))
})
