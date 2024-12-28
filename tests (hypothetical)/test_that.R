# ------------------------------------------------------------------------------
# test_that.R
# Basic testthat suite to confirm that "1+1=1" holds in our IdempotentSemiring
# and synergy code behaves as intended.
# ------------------------------------------------------------------------------
library(testthat)
library(R6)

source("../core/IdempotentArithmetic.R")

test_that("IdempotentArithmetic works as intended", {
  semiring <- IdempotentSemiring$new()
  
  expect_equal(semiring$plus(1,1), 1)
  expect_equal(semiring$plus(1,0), 1)
  expect_equal(semiring$plus(0,0), 0)
  
  expect_equal(semiring$times(1,1), 1)
  expect_equal(semiring$times(1,0), 0)
  expect_equal(semiring$times(0,0), 0)
})

test_that("plus_idem and times_idem vectorized ops hold synergy principle", {
  x <- c(1,0,1)
  y <- c(1,1,0)
  
  expect_equal(plus_idem(x,y), c(1,1,1))
  expect_equal(times_idem(x,y), c(1,0,0))
})

test_that("duality_loss is zero if a+b=1", {
  loss_val <- duality_loss(0.5,0.5,1)
  expect_true(abs(loss_val) < 1e-12)  # effectively zero, floating tolerance
})
