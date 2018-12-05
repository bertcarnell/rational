context("test-rational-class")

test_that("S4 creation works", {
  a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S4")
  expect_equal(a[2]@n, 5L)
  expect_equal(a[2:3]@n, c(5,6))

  a[2] <- rational(7L, 10L, "S4")
  expect_equal(a[2]@n, 7L)
  expect_equal(a[2]@d, 10L)

  a[[2]] <- rational(1L, 6L, "S4")
  expect_equal(a[2]@n, 1L)
  expect_equal(a[2]@d, 6L)

  # the numerator and denominator must be of equal length
  #expect_error(new("rationalS4", as.integer(c(2,3)), as.integer(c(4,5,6))))
  # NULL is not permitted
  expect_error(new("rationalS4", as.integer(3), NULL))
  # numerics are not permitted where integers are required
  expect_error(new("rationalS4", 3, 4))
  # other objects like characters are not permitted
  expect_error(new("rationalS4", "a", as.integer(4)))

  a <- new("rationalS4", as.integer(3), as.integer(4))
  b <- rational(as.integer(3), as.integer(4), "S4")
  expect_true(a@v == b@v && a@n == b@n &&
              a@d == b@d)
  b <- rational(3L, 4L, "S4")
  expect_true(a@v == b@v && a@n == b@n &&
              a@d == b@d)
  b <- rational(3L, as.integer(4), "S4")
  expect_true(a@v == b@v && a@n == b@n &&
              a@d == b@d)
  b <- rational(as.integer(3), 4L, "S4")
  expect_true(a@v == b@v && a@n == b@n &&
              a@d == b@d)

  #expect_error(rational(as.integer(NA), as.integer(3), "S4"))
  expect_error(rational(as.integer(3), as.integer(NA), "S4"))
  expect_error(rational(as.integer(c(2,3)), as.integer(c(4,5,6)), "S4"))
  expect_error(rational(as.integer(3), NULL, "S4"))
  expect_error(rational("a", as.integer(4), "S4"))

  a <- rational(3L, 4L, "S4")
  expect_equal(length(a), 1)
  a <- rational(c(3L,4L), c(5L,6L), "S4")
  expect_equal(length(a), 2)

  expect_equal(a[1]@v, 0.6)
  expect_equal(a[2]@v, 4/6)
  expect_equal(a[1:2]@v, c(0.6, 4/6))
})

test_that("Other arithmetic operations", {
  a <- rational(4L, 5L, "S4")
  b <- rational(2L, 3L, "S4")

  d <- a + b
  expect_equal(d@v, 4/5 + 2/3)
  d <- a - b
  expect_equal(d@v, 4/5 - 2/3)
  d <- a * b
  expect_equal(d@v, (4/5) * (2/3))
  d <- a / b
  expect_equal(d@v, (4/5) / (2/3))
  d <- a %/% b
  expect_equal(d, 1)

  expect_equal(rational(5L,3L,"S4") %/% rational(2L,3L,"S4"), 2)
  expect_equal(rational(5L,4L,"S4") %/% rational(3L,5L,"S4"), 2)
  expect_equal(rational(25L,5L,"S4") %/% rational(4L,1L,"S4"), 1)
  expect_equal(rational(80L,5L,"S4") %/% rational(7L,2L,"S4"), 4)
  expect_equal(rational(5L,1L,"S4") %/% rational(2L,1L,"S4"), 2)
  d <- a %% b
  expect_equal(d@v, 2/15)

  d <- a + 3L
  expect_equal(d@v, (4/5) + 3)
  d <- a + rational(3L, 1L, "S4")
  expect_equal(d@v, (4/5) + 3)
  d <- a + 3.3
  #expect_equal(d, (4/5) + 3.3)
  d <- a - 3L
  expect_equal(d@v, (4/5) - 3)
  d <- a * 3L
  expect_equal(d@v, (4/5) * 3)
  d <- a / 3L
  expect_equal(d@v, (4/5) / 3)
  d <- a %/% 2L
  expect_equal(d, 0)
  d <- a %% 2L
  expect_equal(a@v, d@v)

  d <- a + as.integer(3)
  expect_equal(d@v, (4/5) + 3)
  d <- a - as.integer(3)
  expect_equal(d@v, (4/5) - 3)
  d <- a * as.integer(3)
  expect_equal(d@v, (4/5) * 3)
  d <- a / as.integer(3)
  expect_equal(d@v, (4/5) / 3)
  d <- a %/% as.integer(2)
  expect_equal(d, 0)
  d <- a %% as.integer(2)
  expect_equal(a@v, d@v)

  d <- a^b
  expect_equal(d, (4/5)^(2/3))
  d <- a^3L
  expect_equal(d@v, 4*4*4/5/5/5)
  d <- a^as.integer(3)
  expect_equal(d@v, 4*4*4/5/5/5)

})

test_that("Test comparison", {
  a <- rational(4L, 5L, "S4")
  b <- rational(2L, 3L, "S4")
  expect_true(a > b)
  expect_true(!(a < b))
  expect_true(a >= b)
  expect_true(!(a <= b))
  expect_true(!(a == b))
  expect_true(a != b)

  expect_true(!(a > 0.8))
  expect_true(!(a < 0.8))
  expect_true(a >= 0.8)
  expect_true(a <= 0.8)
  expect_true(a == 0.8)
  expect_true(!(a != 0.8))

  expect_true(!(a > 2))
  expect_true(a < 2)
  expect_true(!(a >= 2))
  expect_true(a <= 2)
  expect_true(!(a == 2))
  expect_true(a != 2)

  expect_true(!(a > as.integer(1)))
  expect_true(a < as.integer(1))
  expect_true(!(a >= as.integer(1)))
  expect_true(a <= as.integer(1))
  expect_true(!(a == as.integer(1)))
  expect_true(a != as.integer(1))

  expect_true(rational(1L,1L, "S4") == as.integer(1))
  expect_true(rational(1L,1L, "S4") == 1)
})

test_that("Other math functions", {
  a <- rational(-1L, 2L, "S4")

  d <- abs(a)
  expect_equal(d@v, 0.5)
  expect_equal(sign(a), -1)

  a <- rational(4L, 5L, "S4")
  b <- rational(3L, 2L, "S4")
  expect_equal(sqrt(a), sqrt(4/5))
  expect_equal(ceiling(a), 1)
  expect_equal(floor(a), 0)
  expect_equal(trunc(a), 0)

  expect_equal(log(a), log(4/5))
  expect_equal(log10(a), log10(4/5))
  expect_equal(log2(a), log2(4/5))
  expect_equal(log1p(a), log1p(4/5))
  expect_equal(logb(a, base = 8), logb(4/5, base = 8))

  expect_equal(acos(a), acos(4/5))
  expect_equal(acosh(b), acosh(3/2))
  expect_equal(asin(a), asin(4/5))
  expect_equal(asinh(a), asinh(4/5))
  expect_equal(atan(a), atan(4/5))
  expect_equal(atanh(a), atanh(4/5))
  expect_equal(cos(a), cos(4/5))
  expect_equal(cosh(a), cosh(4/5))
  expect_equal(sin(a), sin(4/5))
  expect_equal(sinh(a), sinh(4/5))
  expect_equal(tan(a), tan(4/5))
  expect_equal(tanh(a), tanh(4/5))
  expect_equal(exp(a), exp(4/5))
  expect_equal(expm1(a), expm1(4/5))

  expect_equal(gamma(a), gamma(4/5))
  expect_equal(lgamma(a), lgamma(4/5))
  expect_equal(digamma(a), digamma(4/5))
  expect_equal(trigamma(a), trigamma(4/5))

  expect_equal(gamma(rational(1L,2L, "S4")), gamma(0.5))
  expect_equal(gamma(rational(3L,2L, "S4")), gamma(1.5))
  expect_equal(gamma(rational(5L,2L, "S4")), gamma(2.5))
  expect_equal(gamma(rational(7L,2L, "S4")), gamma(3.5))

  #a <- rational(1, 2)
  #b <- rational(2, 4)

  #expect_equal(a + b, 1)
  #expect_true(a == b)

  # cum functions are part of the Math group and need work
  #a <- rational(4,5,"S4")
  #b <- rational(3,4,"S4")
  #d <- rational(c(2,3,4,5), c(3,4,5,6),"S4")
  #d <- cumsum(a)
  #print(d)
  #expect_equal(d, 4/5)

  #cummax(a)
  #cummin(a)
  #cumprod(a)
  #cumsum(a)

  expect_true(!(0.1 + 0.1 + 0.1 == 0.3))
  expect_true(sum(rational(c(1L,1L,1L),c(10L,10L,10L), "S4")) == 0.3)
  expect_true(sum(rational(c(1L,1L,1L),c(10L,10L,10L), "S4")) == rational(3L,10L, "S4"))
  expect_true(max(rational(c(2L,3L,4L,1L),c(5L,7L,3L,2L), "S4")) == rational(4L,3L, "S4"))
  expect_true(min(rational(c(2L,3L,4L,1L),c(5L,7L,3L,2L), "S4")) == rational(2L,5L, "S4"))
  d <- range(rational(c(2L,3L,4L,1L),c(5L,7L,3L,2L), "S4"))
  expect_equal(d[[1]]@v, 2/5)
  expect_equal(d[[2]]@v, 4/3)
  expect_true(d[[1]] == rational(2L,5L, "S4"))
  expect_true(d[[2]] == rational(4L,3L, "S4"))
  expect_true(prod(rational(c(1L,2L,3L), c(10L,10L,10L), "S4")) == rational(6L,1000L, "S4"))

  a <- as.rationalS4(0.3)
  expect_equal(a@n, 3)
  expect_equal(a@d, 10)
  #expect_equal(a, rational(3L,10L,"S4"))

  expect_warning(as.rationalS4(c(0.3,0.3333)))
  suppressWarnings(b <- as.rationalS4(c(0.3,0.3333)))
  #expect_equal(b[1], rational(3L,10L, "S4"))
  #expect_equal(b[2], rational(1L,3L, "S4"))
  expect_equal(attr(a, "abs.error"), 0)
  expect_equal(attr(b, "abs.error"), c(0, 1/3 - 0.3333))

  a <- as.rationalS4(0.3333, cycles = 20, max.denominator = 100000)
  #expect_equal(a, rational(3333L, 10000L, "S4"))
  expect_equal(a@n, 3333)
  expect_equal(a@d, 10000)
  expect_equal(attr(a, "abs.error"), 0)

  a <- as.rationalS4("0.3")
  expect_equal(a@n, 3)
  expect_equal(a@d, 10)
  #expect_equal(a, rational(3L,10L, "S4"))
})
