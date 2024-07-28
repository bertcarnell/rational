testthat::context("test-rational-subtract")

testthat::test_that("Test Subtract S4", {
  a <- rational(1L,2L,"S4")
  b <- rational(3L,5L,"S4")
  d <- a - b
  expect_true(d@n == -1)
  expect_true(d@d == 10)
  a <- 7L
  b <- rational(3L,5L,"S4")
  d <- a - b
  expect_true(d@n == 32)
  expect_true(d@d == 5)
  a <- rational(1L,2L,"S4")
  b <- 7L
  d <- a - b
  expect_true(d@n == -13)
  expect_true(d@d == 2)
  a <- 7
  b <- rational(3L,5L,"S4")
  d <- a - b
  expect_true(abs(d - 6.4) < 1E-12)
  a <- rational(1L,2L,"S4")
  b <- 7
  d <- a - b
  expect_true(abs(d - -6.5) < 1E-12)
})

testthat::test_that("Test Subtract S3", {
  expect_true(is.na(rational(1L, 2L, "S3") - "test"))

  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- a - b
  expect_true(d$n == -1)
  expect_true(d$d == 10)
  a <- 7L
  b <- rational(3L,5L,"S3")
  d <- a - b
  expect_true(d$n == 32)
  expect_true(d$d == 5)
  a <- rational(1L,2L,"S3")
  b <- 7L
  d <- a - b
  expect_true(d$n == -13)
  expect_true(d$d == 2)
  a <- 7
  b <- rational(3L,5L,"S3")
  d <- a - b
  expect_true(abs(d - 6.4) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- 7
  d <- a - b
  expect_true(abs(d - -6.5) < 1E-12)
})

testthat::test_that("Rational Subtract R6", {
  expect_true(is.na(rational(1L, 2L, "R6") - "test"))

  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- a - b
  expect_true(d$getNumerator() == -1)
  expect_true(d$getDenominator() == 10)
  a <- 7L
  b <- rational(3L,5L,"R6")
  d <- a - b
  expect_true(d$getNumerator() == 32)
  expect_true(d$getDenominator() == 5)
  a <- rational(1L,2L,"R6")
  b <- 7L
  d <- a - b
  expect_true(d$getNumerator() == -13)
  expect_true(d$getDenominator() == 2)
  a <- 7
  b <- rational(3L,5L,"R6")
  d <- a - b
  expect_true(abs(d - 6.4) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- 7
  d <- a - b
  expect_true(abs(d - -6.5) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  a$subtract(b)
  expect_true(a$getNumerator() == -1)
  expect_true(a$getDenominator() == 10)
  a <- rational(1L,2L,"R6")
  b <- 7L
  a$subtract(b)
  expect_true(a$getNumerator() == -13)
  expect_true(a$getDenominator() == 2)

})

testthat::test_that("Test Subtract S7", {
  a <- rational(1L,2L,"S7")
  b <- rational(3L,5L,"S7")
  d <- a - b
  expect_true(d@n == -1)
  expect_true(d@d == 10)
  a <- 7L
  b <- rational(3L,5L,"S7")
  d <- a - b
  expect_true(d@n == 32)
  expect_true(d@d == 5)
  a <- rational(1L,2L,"S7")
  b <- 7L
  d <- a - b
  expect_true(d@n == -13)
  expect_true(d@d == 2)
  a <- 7
  b <- rational(3L,5L,"S7")
  d <- a - b
  expect_true(abs(d - 6.4) < 1E-12)
  a <- rational(1L,2L,"S7")
  b <- 7
  d <- a - b
  expect_true(abs(d - -6.5) < 1E-12)
})
