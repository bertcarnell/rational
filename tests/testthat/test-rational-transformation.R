testthat::context("test-rational-transformation")

testthat::test_that("Test transformation", {
  expect_equal(c(2/5, 3), as.numeric(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_equal(c(0L, 3L), as.integer(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_equal(2L, as.integer(rational(8L, 3L, "S4")))
  expect_equal(c("2 / 5", "3 / 1"), as.character(rational(c(2L, 3L), c(5L, 1L), "S4")))
  expect_true(rational(333L, 10L, "S4") == as.rationalS4(33.3))
})
