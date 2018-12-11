testthat::context("test-rational-compare")

testthat::test_that("Test S3", {
  a <- rational(1L, 3L, "S3")
  b <- rational(3L, 4L, "S3")
  d <- 3L
  e <-  20.1
  expect_true(a != b)
  expect_true(!(a == b))
  expect_true(a < b)
  expect_true(!(a > b))
  expect_true(a <= b)
  expect_true(!(a >= b))
  expect_true(a != d)
  expect_true(!(a == d))
  expect_true(a < d)
  expect_true(!(a > d))
  expect_true(a <= d)
  expect_true(!(a >= d))
  expect_true(a != e)
  expect_true(!(a == e))
  expect_true(a < e)
  expect_true(!(a > e))
  expect_true(a <= e)
  expect_true(!(a >= e))

  expect_true(d != a)

  expect_error(rational(c(1L, 2L), c(3L, 5L), "S3") == rational(6L, 7L, "S3"))

  expect_error(a == "test")
})

testthat::test_that("Test S4", {
  a <- rational(1L, 3L, "S4")
  b <- rational(3L, 4L, "S4")
  d <- 3L
  e <-  20.1
  expect_true(a != b)
  expect_true(!(a == b))
  expect_true(a < b)
  expect_true(!(a > b))
  expect_true(a <= b)
  expect_true(!(a >= b))
  expect_true(a != d)
  expect_true(!(a == d))
  expect_true(a < d)
  expect_true(!(a > d))
  expect_true(a <= d)
  expect_true(!(a >= d))
  expect_true(a != e)
  expect_true(!(a == e))
  expect_true(a < e)
  expect_true(!(a > e))
  expect_true(a <= e)
  expect_true(!(a >= e))

  expect_true(d != a)
})

testthat::test_that("Test R6", {
  a <- rational(1L, 3L, "R6")
  b <- rational(3L, 4L, "R6")
  d <- 3L
  e <-  20.1
  expect_true(a != b)
  expect_true(!(a == b))
  expect_true(a < b)
  expect_true(!(a > b))
  expect_true(a <= b)
  expect_true(!(a >= b))
  expect_true(a != d)
  expect_true(!(a == d))
  expect_true(a < d)
  expect_true(!(a > d))
  expect_true(a <= d)
  expect_true(!(a >= d))
  expect_true(a != e)
  expect_true(!(a == e))
  expect_true(a < e)
  expect_true(!(a > e))
  expect_true(a <= e)
  expect_true(!(a >= e))

  expect_true(d != a)

  expect_error(rational(c(1L, 2L), c(3L, 5L), "R6") == rational(6L, 7L, "R6"))

  expect_error(a == "test")
})
