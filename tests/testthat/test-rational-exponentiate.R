context("test-rational-exponentiate")

test_that("exponentiations works", {
  expect_error(.integerExponentiate(c(2L,5), c(6,7)))
  expect_equal(.integerExponentiate(2L, 5L), 32L)
  expect_equal(.integerExponentiate(2L, 1L), 2L)
  expect_true(is.infinite(.integerExponentiate(2L, 2000000L, checkOverflow = TRUE)))
  expect_equal(.integerExponentiate(c(2L,3L), c(5L,3L)), c(32L,27L))
  expect_equal(2147483640^2, .integerExpRational(2147483640L, 2L, 1L, 2))
  expect_true(is.na(rational(1L, 2L, "S3")^"test"))
  expect_true(is.na(rational(1L, 2L, "R6")^"test"))

  for (i in c("S4","S3","R6"))
  {
    expect_equal(rational(2L, 3L, i)^rational(5L, 1L, i), rational(32L, 243L, i))
    expect_equal(rational(2L, 3L, i)^rational(5L, 2L, i), (2/3)^(5/2))
    expect_equal(rational(2L, 3L, i)^rational(500000000L, 1L, i), (2/3)^(500000000L/1))
    expect_equal(rational(c(2L, 3L), c(3L, 4L), i) ^ rational(2L, 1L, i), rational(c(4L,9L), c(9L,16L),i))

    expect_equal(rational(2L, 3L, i)^5L, rational(32L, 243L, i))
    expect_equal(rational(2L, 3L, i)^50000L, (2/3)^50000L)
    expect_equal(rational(c(2L, 3L), c(3L, 4L), i) ^ 2L, rational(c(4L,9L),c(9L,16L), i))

    expect_equal(5L^rational(2L, 3L, i), 5^(2/3))
    expect_equal(5L^rational(2L, 1L, i), rational(25L, 1L, i))

    expect_equal(rational(1L, 2L, i)^1.5, (1/2)^1.5)
    expect_equal(1.7^rational(1L, 2L, i), 1.7^(1/2))
  }

  for (i in c("S4","S3","R6"))
  {
    a <- rational(1L,2L,i)
    b <- rational(3L,5L,i)
    d <- 3L
    e <- 1.5
    expect_equal(a ^ b, (1/2)^(3/5))
    expect_equal(d ^ b, 3^(3/5))
    expect_equal(a ^ d, rational(1L, 8L, i))
    expect_equal(e ^ b, 1.5 ^ (3/5))
    expect_equal(d ^ e, 3^1.5)
    expect_equal(a ^ e, (1/2)^1.5)
  }
})

test_that("Exponentiation with vectors works", {
  # both rational
  a <- rational(c(1L, 3L), c(5L, 6L), "R6")
  b <- rational(c(2L, 2L), c(3L, 5L), "R6")
  expect_equal(a^b, c((1/5)^(2/3), (3/6)^(2/5)))
  # both rational with one denominator == 1
  a <- rational(c(1L, 3L), c(5L, 6L), "R6")
  b <- rational(c(2L, 2L), c(3L, 1L), "R6")
  expect_equal(a^b, c((1/5)^(2/3), (3/6)^(2/1)))
  # both rational with both denominator == 1
  a <- rational(c(1L, 3L), c(5L, 6L), "R6")
  b <- rational(c(2L, 2L), c(1L, 1L), "R6")
  expect_equal(a^b, rational(c(1L, 9L), c(25L, 36L)))
  # integer overflow
  a <- rational(c(2000000000L, 2000000000L), c(2L, 2L), "R6")
  b <- rational(c(8L, 9L), c(3L, 4L), "R6")
  expect_equal(a^b, c((2E9/2)^(8/3), (2E9/2)^(9/4)))
})


