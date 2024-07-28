testthat::context("test-rational-integer-division")

testthat::test_that("Test Integer Division S4", {
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
})

testthat::test_that("Test Integer Division S3", {
  expect_true(is.na(rational(1L, 2L, "S3") %/% "test"))

  a <- rational(1L,2L,"S3")
  b <- rational(3L,5L,"S3")
  d <- 3L
  e <- 1.5
  expect_equal(0, a %/% b)
  expect_equal(5, d %/% b)
  expect_equal(0, a %/% d)
  expect_equal(2, e %/% b)
  expect_equal(2, d %/% e)

  expect_true(is.na(rational(1L, 2L, "R6") %/% "test"))
})

testthat::test_that("Test Integer Division R6", {
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

testthat::test_that("Test Integer Division S7", {
  a <- rational(1L,2L,"S7")
  b <- rational(3L,5L,"S7")
  d <- 3L
  e <- 1.5
  expect_equal(0, a %/% b)
  expect_equal(5, d %/% b)
  expect_equal(0, a %/% d)
  expect_equal(2, e %/% b)
  expect_equal(2, d %/% e)
})
