context("test-rational-extract")

testthat::test_that("Test Extract S4", {
  a <- rational(c(1L,2L,3L),c(4L,5L,7L), "S4")
  b <- rational(2L, 5L, "S4")
  d <- rational(1L, 9L, "S4")
  expect_true(a[2] == b)
  expect_true(a[[2]] == b)
  a[3] <- d
  a[[1]] <- d
  expect_true(a[[3]] == d)
  expect_true(a[1] == d)
  expect_true(a[2] == b)
})

testthat::test_that("Test Extract S3", {
  a <- rational(c(1L,2L,3L), c(4L,5L,7L), "S3")
  b <- rational(2L, 5L, "S3")
  d <- rational(1L, 9L, "S3")
  expect_true(a[2] == b)
  expect_true(a[[2]] == b)
  a[3] <- d
  a[[1]] <- d
  expect_true(a[[3]] == d)
  expect_true(a[1] == d)
  expect_true(a[2] == b)

  expect_error(a[3] <- 7)
  expect_error(a[[3]] <- "5")
})

testthat::test_that("Test Extract R6", {
  a <- rational(c(1L,2L,3L), c(4L,5L,7L), "R6")
  b <- rational(2L, 5L, "R6")
  d <- rational(1L, 9L, "R6")
  expect_true(a[2] == b)
  expect_true(a[[2]] == b)
  a$assign_at(3, d)
  a$assign_at(1, d)
  expect_true(a[[3]] == d)
  expect_true(a[1] == d)
  expect_true(a[2] == b)

  a <- rational(c(1L,2L,3L), c(4L,5L,7L), "R6")
  a[3] <- d
  expect_true(a[[3]] == d)
  expect_true(a[3] == d)
  a[[1]] <- d
  expect_true(a[[1]] == d)
  expect_true(a[1] == d)

  expect_error(a[2] <- "a")
  expect_error(a[[2]] <- "b")
})

testthat::test_that("Test Extract S7", {
  a <- rational(c(1L,2L,3L), c(4L,5L,7L), "S7")
  b <- rational(2L, 5L, "S7")
  d <- rational(1L, 9L, "S7")
  expect_true(a[2]@n == b@n)
  expect_true(a[2]@d == b@d)
  expect_true(a[[2]]@n == b@n)
  expect_true(a[[2]]@d == b@d)

  a[3] <- d
  expect_true(a[[3]]@n == d@n)
  expect_true(a[[3]]@d == d@d)
  expect_true(a[3]@n == d@n)
  expect_true(a[3]@d == d@d)
  a[[1]] <- d
  expect_true(a[[1]]@n == d@n)
  expect_true(a[[1]]@d == d@d)
  expect_true(a[1]@n == d@n)
  expect_true(a[1]@d == d@d)

  expect_error(a[2] <- "a")
  expect_error(a[[2]] <- "b")
})
