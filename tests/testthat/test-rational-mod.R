testthat::context("test-rational-mod")

testthat::test_that("Test Mod S4", {
  a <- rational(5L, 1L, "S4")
  b <- rational(2L, 1L, "S4")
  d <- rational(3L, 2L, "S4")
  e <- rational(1L, 2L, "S4")
  expect_true(rational(1L, 1L, "S4") == a %% b)
  expect_true(rational(0L, 1L, "S4") == d %% e)
  expect_true(rational(1L, 1L, "S4") == 5L %% b)
  expect_true(rational(1L, 1L, "S4") == a %% 2L)
  expect_true(rational(0L, 1L, "S4") == 2L %% e)
  expect_equal(1, 5 %% b)
  expect_equal(1, a %% 2)
})

testthat::test_that("Test Mod S3", {
  a <- rational(5L, 1L, "S3")
  b <- rational(2L, 1L, "S3")
  d <- rational(3L, 2L, "S3")
  e <- rational(1L, 2L, "S3")
  expect_true(rational(1L, 1L, "S3") == a %% b)
  expect_true(rational(0L, 1L, "S3") == d %% e)
  expect_true(rational(1L, 1L, "S3") == 5L %% b)
  expect_true(rational(1L, 1L, "S3") == a %% 2L)
  expect_true(rational(0L, 1L, "S3") == 2L %% e)
  expect_equal(1, 5 %% b)
  expect_equal(1, a %% 2)

  f <- rational(5L, 2L, "S3")
  g <- rational(11L, 10L, "S3")
  # 5/2 - 11/10 * (5/2 %/% 11/10) = 5/2 - 11/10 * 2 = 25/10 - 22/10 = 3/10
  expect_true(rational(3L, 10L, "S3") == f %% g)
})

testthat::test_that("Test Mod R6", {
  a <- rational(5L, 1L, "R6")
  b <- rational(2L, 1L, "R6")
  d <- rational(3L, 2L, "R6")
  e <- rational(1L, 2L, "R6")
  expect_true(rational(1L, 1L, "R6") == a %% b)
  expect_true(rational(0L, 1L, "R6") == d %% e)
  expect_true(rational(1L, 1L, "R6") == 5L %% b)
  expect_true(rational(1L, 1L, "R6") == a %% 2L)
  expect_true(rational(0L, 1L, "R6") == 2L %% e)
  expect_equal(1, 5 %% b)
  expect_equal(1, a %% 2)

  f <- rational(5L, 2L, "R6")
  g <- rational(11L, 10L, "R6")
  # 5/2 - 11/10 * (5/2 %/% 11/10) = 5/2 - 11/10 * 2 = 25/10 - 22/10 = 3/10
  expect_true(rational(3L, 10L, "R6") == f %% g)
})

testthat::test_that("Test Mod S7", {
  a <- rational(5L, 1L, "S7")
  b <- rational(2L, 1L, "S7")
  d <- rational(3L, 2L, "S7")
  e <- rational(1L, 2L, "S7")
  expect_true(rational(1L, 1L, "S7") == a %% b)
  expect_true(rational(0L, 1L, "S7") == d %% e)
  expect_true(rational(1L, 1L, "S7") == 5L %% b)
  expect_true(rational(1L, 1L, "S7") == a %% 2L)
  expect_true(rational(0L, 1L, "S7") == 2L %% e)
  expect_equal(1, 5 %% b)
  expect_equal(1, a %% 2)

  f <- rational(5L, 2L, "S7")
  g <- rational(11L, 10L, "S7")
  # 5/2 - 11/10 * (5/2 %/% 11/10) = 5/2 - 11/10 * 2 = 25/10 - 22/10 = 3/10
  expect_true(rational(3L, 10L, "S7") == f %% g)
})
