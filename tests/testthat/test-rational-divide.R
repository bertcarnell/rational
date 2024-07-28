testthat::context("test-rational-divide")

testthat::test_that("Test Divide S4", {
  a <- rational(1L,2L,"S4")
  b <- rational(3L,5L,"S4")
  d <- a / b
  expect_true(d@n == 5)
  expect_true(d@d == 6)
  a <- 7L
  b <- rational(3L,5L,"S4")
  d <- a / b
  expect_true(d@n == 35)
  expect_true(d@d == 3)
  a <- rational(1L,2L,"S4")
  b <- 7L
  d <- a / b
  expect_true(d@n == 1)
  expect_true(d@d == 14)
  a <- 7
  b <- rational(3L,5L,"S4")
  d <- a / b
  expect_true(abs(d - 7*5/3) < 1E-12)
  a <- rational(1L,2L,"S4")
  b <- 7
  d <- a / b
  expect_true(abs(d - 1/14) < 1E-12)
})

testthat::test_that("Test divide S3", {
  expect_true(is.na(rational(1L, 2L, "S3") / "test"))

  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- a / b
  expect_true(d$n == 5)
  expect_true(d$d == 6)
  a <- 7L
  b <- rational(3L,5L,"S3")
  d <- a / b
  expect_true(d$n == 35)
  expect_true(d$d == 3)
  a <- rational(1L,2L,"S3")
  b <- 7L
  d <- a / b
  expect_true(d$n == 1)
  expect_true(d$d == 14)
  a <- 7
  b <- rational(3L,5L,"S3")
  d <- a / b
  expect_true(abs(d - 7*5/3) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- 7
  d <- a / b
  expect_true(abs(d - 1/14) < 1E-12)
})

testthat::test_that("Test Divide R6", {
  expect_true(is.na(rational(1L, 2L, "R6") / "test"))

  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- a / b
  expect_true(d$getNumerator() == 5)
  expect_true(d$getDenominator() == 6)
  a <- 7L
  b <- rational(3L,5L,"R6")
  d <- a / b
  expect_true(d$getNumerator() == 35)
  expect_true(d$getDenominator() == 3)
  a <- rational(1L,2L,"R6")
  b <- 7L
  d <- a / b
  expect_true(d$getNumerator() == 1)
  expect_true(d$getDenominator() == 14)
  a <- 7
  b <- rational(3L,5L,"R6")
  d <- a / b
  expect_true(abs(d - 7*5/3) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- 7
  d <- a / b
  expect_true(abs(d - 1/14) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  a$divide(b)
  expect_true(a$getNumerator() == 5)
  expect_true(a$getDenominator() == 6)
  a <- rational(1L,2L,"R6")
  b <- 7L
  a$divide(b)
  expect_true(a$getNumerator() == 1)
  expect_true(a$getDenominator() == 14)
})

testthat::test_that("Test Divide S7", {
  a <- rational(1L,2L,"S7")
  b <- rational(3L,5L,"S7")
  d <- a / b
  expect_true(d@n == 5)
  expect_true(d@d == 6)
  a <- 7L
  b <- rational(3L,5L,"S7")
  d <- a / b
  expect_true(d@n == 35)
  expect_true(d@d == 3)
  a <- rational(1L,2L,"S7")
  b <- 7L
  d <- a / b
  expect_true(d@n == 1)
  expect_true(d@d == 14)
  a <- 7
  b <- rational(3L,5L,"S7")
  d <- a / b
  expect_true(abs(d - 7*5/3) < 1E-12)
  a <- rational(1L,2L,"S7")
  b <- 7
  d <- a / b
  expect_true(abs(d - 1/14) < 1E-12)
})
