# rational

A rational number class using a variety of class systems

[![R-CMD-CHECK](https://github.com/bertcarnell/rational/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/bertcarnell/rational/actions/workflows/r-cmd-check.yml)
[![Coverage status](https://codecov.io/gh/bertcarnell/rational/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/rational?branch=master)

[Rational Website](https://bertcarnell.github.io/rational/)

## Why a Rational Package?

This package serves 2 purposes:

- Demonstrates the creation of the same classes in S3, S4, and R6 class systems
- Demonstrates one way to solve some numerical accuracy problems that people struggle with

```r
> # expectations dashed
> (0.1 + 0.2) == 0.3
[1] FALSE
> # what?
> print(0.1 + 0.2, digits = 20)
[1] 0.30000000000000004
> # what am I supposed to do in R? (or any other floating point arithmetic system)
> all.equal(0.1 + 0.2, 0.3, tolerance = 1E-9)
[1] TRUE
> abs(0.1 + 0.2 - 0.3) < 1E-9
[1] TRUE
> # is there another way?  Yes, rational numbers
> #   NOTE: the "L" notation indicates an integer
> rational(1L, 10L) + rational(2L, 10L) == rational(3L, 10L)
[1] TRUE
```
