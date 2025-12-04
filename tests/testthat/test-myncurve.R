library(testthat)

# Test 1: Return type and elements
test_that("myncurve returns a list with expected elements", {
  result <- myncurve(mu=0, sigma=1, a=1)
  expect_type(result, "list")
  expect_named(result, c("mu", "sigma", "a", "P_Y_le_a"))
})

# Test 2: Known probability
test_that("myncurve calculates correct probability", {
  result <- myncurve(mu=0, sigma=1, a=0)
  expect_equal(result$P_Y_le_a, 0.5, tolerance = 1e-8)
})

# Test 3: Probability between 0 and 1
test_that("myncurve probability is between 0 and 1", {
  result <- myncurve(mu=5, sigma=2, a=6)
  expect_true(result$P_Y_le_a >=0)
  expect_true(result$P_Y_le_a <= 1)
})
