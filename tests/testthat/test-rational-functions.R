testthat::context("test-rational-functions")

testthat::test_that("Test S4 functions", {
  a <- rational(5L, 2L, "S4")
  expect_equal(2, floor(a))
  expect_equal(-1, sign(rational(-1L, 2L, "S4")))
  expect_equal(2, round(rational(5L, 2L, "S4"), 0))
  x <- abs(rational(-2L, 3L, "S4"))
  expect_true("rationalS4" %in% class(x))
  expect_true(2/3 == abs(x))
  b <- rational(2L, 5L, "S4")
  expect_equal(log(2/5), log(b))
  expect_equal(log10(2/5), log10(b))
  expect_equal(logb(2/5, base = 5), logb(b, base = 5))
  expect_equal(log2(2/5), log2(b))
  expect_equal(gamma(2/5), gamma(b))
  expect_true(3 == max(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_true(2/5 == min(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_true(is.list(range(rational(c(2L, 3L), c(5L, 1L), "S4"))))
  expect_true(6/5 == prod(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_true(17/5 == sum(rational(c(2L, 3L), c(5L, 1L), "S4")))
})