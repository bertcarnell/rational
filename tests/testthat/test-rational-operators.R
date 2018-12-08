testthat::context("test-rational-operators")

testthat::test_that("Test Addition", {
  a <- rational(1L,2L,"S4")
  b <- rational(3L,5L,"S4")
  d <- a + b
  expect_true(d@n == 11)
  expect_true(d@d == 10)
  a <- 7L
  b <- rational(3L,5L,"S4")
  d <- a + b
  expect_true(d@n == 38)
  expect_true(d@d == 5)
  a <- rational(1L,2L,"S4")
  b <- 7L
  d <- a + b
  expect_true(d@n == 15)
  expect_true(d@d == 2)
  a <- 7
  b <- rational(3L,5L,"S4")
  d <- a + b
  expect_true(abs(d - 7.6) < 1E-12)
  a <- rational(1L,2L,"S4")
  b <- 7
  d <- a + b
  expect_true(abs(d - 7.5) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- a + b
  expect_true(d$n == 11)
  expect_true(d$d == 10)
  a <- 7L
  b <- rational(3L,5L,"S3")
  d <- a + b
  expect_true(d$n == 38)
  expect_true(d$d == 5)
  a <- rational(1L,2L,"S3")
  b <- 7L
  d <- a + b
  expect_true(d$n == 15)
  expect_true(d$d == 2)
  a <- 7
  b <- rational(3L,5L,"S3")
  d <- a + b
  expect_true(abs(d - 7.6) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- 7
  d <- a + b
  expect_true(abs(d - 7.5) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- a + b
  expect_true(d$getNumerator() == 11)
  expect_true(d$getDenominator() == 10)
  a <- 7L
  b <- rational(3L,5L,"R6")
  d <- a + b
  expect_true(d$getNumerator() == 38)
  expect_true(d$getDenominator() == 5)
  a <- rational(1L,2L,"R6")
  b <- 7L
  d <- a + b
  expect_true(d$getNumerator() == 15)
  expect_true(d$getDenominator() == 2)
  a <- 7
  b <- rational(3L,5L,"R6")
  d <- a + b
  expect_true(abs(d - 7.6) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- 7
  d <- a + b
  expect_true(abs(d - 7.5) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  a$add(b)
  expect_true(a$getNumerator() == 11)
  expect_true(a$getDenominator() == 10)
  a <- rational(1L,2L,"R6")
  b <- 7L
  a$add(b)
  expect_true(a$getNumerator() == 15)
  expect_true(a$getDenominator() == 2)
})

testthat::test_that("Test Multiply", {
  a <- rational(1L,2L,"S4")
  b <- rational(3L,5L,"S4")
  d <- a * b
  expect_true(d@n == 3)
  expect_true(d@d == 10)
  a <- 7L
  b <- rational(3L,5L,"S4")
  d <- a * b
  expect_true(d@n == 21)
  expect_true(d@d == 5)
  a <- rational(1L,2L,"S4")
  b <- 7L
  d <- a * b
  expect_true(d@n == 7)
  expect_true(d@d == 2)
  a <- 7
  b <- rational(3L,5L,"S4")
  d <- a * b
  expect_true(abs(d - 4.2) < 1E-12)
  a <- rational(1L,2L,"S4")
  b <- 7
  d <- a * b
  expect_true(abs(d - 3.5) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- a * b
  expect_true(d$n == 3)
  expect_true(d$d == 10)
  a <- 7L
  b <- rational(3L,5L,"S3")
  d <- a * b
  expect_true(d$n == 21)
  expect_true(d$d == 5)
  a <- rational(1L,2L,"S3")
  b <- 7L
  d <- a * b
  expect_true(d$n == 7)
  expect_true(d$d == 2)
  a <- 7
  b <- rational(3L,5L,"S3")
  d <- a * b
  expect_true(abs(d - 4.2) < 1E-12)
  a <- rational(1L,2L,"S3")
  b <- 7
  d <- a * b
  expect_true(abs(d - 3.5) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- a * b
  expect_true(d$getNumerator() == 3)
  expect_true(d$getDenominator() == 10)
  a <- 7L
  b <- rational(3L,5L,"R6")
  d <- a * b
  expect_true(d$getNumerator() == 21)
  expect_true(d$getDenominator() == 5)
  a <- rational(1L,2L,"R6")
  b <- 7L
  d <- a * b
  expect_true(d$getNumerator() == 7)
  expect_true(d$getDenominator() == 2)
  a <- 7
  b <- rational(3L,5L,"R6")
  d <- a * b
  expect_true(abs(d - 4.2) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- 7
  d <- a * b
  expect_true(abs(d - 3.5) < 1E-12)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  a$multiply(b)
  expect_true(a$getNumerator() == 3)
  expect_true(a$getDenominator() == 10)
  a <- rational(1L,2L,"R6")
  b <- 7L
  a$multiply(b)
  expect_true(a$getNumerator() == 7)
  expect_true(a$getDenominator() == 2)
})

testthat::test_that("Test Divide", {
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

testthat::test_that("Test Subtract", {
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

testthat::test_that("Test Integer Divistion", {
  a <- rational(3L,1L,"S4")
  b <- rational(2L,1L,"S4")
  expect_equal(1, a %/% b)
  a <- 3L
  b <- rational(2L,1L,"S4")
  expect_equal(1, a %/% b)
  a <- rational(3L,1L,"S4")
  b <- 2L
  expect_equal(1, a %/% b)
  a <- 3.5
  b <- rational(2L,1L,"S4")
  expect_equal(1, a %/% b)
  a <- rational(3L,1L,"S4")
  b <- 2.1
  expect_equal(1, a %/% b)
  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- 3L
  e <- 1.5
  expect_equal(0, a %/% b)
  expect_equal(5, d %/% b)
  expect_equal(0, a %/% d)
  expect_equal(2, e %/% b)
  expect_equal(2, d %/% e)
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- 3L
  e <- 1.5
  expect_equal(0, a %/% b)
  expect_equal(5, d %/% b)
  expect_equal(0, a %/% d)
  expect_equal(2, e %/% b)
  expect_equal(2, d %/% e)

})
