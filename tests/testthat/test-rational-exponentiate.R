context("test-rational-exponentiate")

test_that("exponentiations works", {
  expect_equal(.integerExponentiate(2L, 5L), 32L)
  expect_equal(.integerExponentiate(2L, 1L), 2L)
  expect_true(is.infinite(.integerExponentiate(2L, 2000000L, checkOverflow = TRUE)))
  expect_equal(.integerExponentiate(c(2L,3L), c(5L,3L)), c(32L,27L))

  rational(2L, 3L, "S4")^rational(5L, 1L, "S4")
  rational(2L, 3L, "S4")^rational(5L, 2L, "S4")
  rational(2L, 3L, "S4")^rational(500000000L, 1L, "S4")
  rational(c(2L, 3L), c(3L, 4L), "S4") ^ rational(2L, 1L, "S4")

  rational(2L, 3L, "S4")^5L
  rational(2L, 3L, "S4")^50000L
  rational(c(2L, 3L), c(3L, 4L), "S4") ^ 2L

  5L^rational(2L, 3L, "S4")
  5L^rational(2L, 1L, "S4")

  rational(1L, 2L, "S4")^1.5
  1.7^rational(1L, 2L, "S4")

  rational(2L, 3L, "S3")^rational(5L, 1L, "S3")
  rational(2L, 3L, "S3")^rational(5L, 2L, "S3")
  rational(2L, 3L, "S3")^rational(500000000L, 1L, "S3")
  rational(c(2L, 3L), c(3L, 4L), "S3") ^ rational(2L, 1L, "S3")
  rational(2L, 3L, "S3")^5L
  rational(2L, 3L, "S3")^50000L
  rational(c(2L, 3L), c(3L, 4L), "S3") ^ 2L
  5L^rational(2L, 3L, "S3")
  5L^rational(2L, 1L, "S3")
  rational(1L, 2L, "S3")^1.5
  1.7^rational(1L, 2L, "S3")

  rational(2L, 3L, "R6")^rational(5L, 1L, "R6")
  rational(2L, 3L, "R6")^rational(5L, 2L, "R6")
  rational(2L, 3L, "R6")^rational(500000000L, 1L, "R6")
  rational(c(2L, 3L), c(3L, 4L), "R6") ^ rational(2L, 1L, "R6")
  rational(2L, 3L, "R6")^5L
  rational(2L, 3L, "R6")^50000L
  rational(c(2L, 3L), c(3L, 4L), "R6") ^ 2L
  5L^rational(2L, 3L, "R6")
  5L^rational(2L, 1L, "R6")
  rational(1L, 2L, "R6")^1.5
  1.7^rational(1L, 2L, "R6")

  a <- rational(1L,2L,"S4")
  b <- rational(3L,5L,"S4")
  a ^ b
  a <- rational(1L,2L,"S4")
  b <- 3L
  a ^ b
  a <- 2L
  b <- rational(3L,5L,"S4")
  a ^ b
  a <- rational(1L,2L,"S4")
  b <- 1.5
  a ^ b
  a <- 3.1
  b <- rational(3L,5L,"S4")
  a ^ b
  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- 3L
  e <- 1.5
  a ^ b
  d ^ b
  a ^ d
  e ^ b
  d ^ e
  a <- rational(1L,2L,"R6")
  b <- rational(3L,5L,"R6")
  d <- 3L
  e <- 1.5
  a ^ b
  d ^ b
  a ^ d
  e ^ b
  d ^ e
})


