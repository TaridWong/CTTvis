library(testthat)        # load testthat package

devtools::load_all()
library(CTTvis)       # load the package

context("check-output")  # Our file is called "test-check_output.R"

data("dichotomous_response")

#-------------Test difficulty_plot-------------

# Define the test cases for difficulty_plot
test_that("difficulty_plot returns a data frame", {
  result <- difficulty_plot(dichotomous_response)

  # Test if the result is a data frame
  expect_true(is.data.frame(result))

  # Test if the data frame has the correct columns
  expect_named(result, c("item", "difficulty"))
})

test_that("difficulty_plot produces a plot", {
  # Capture plotting output
  expect_silent(plot_output <- difficulty_plot(dichotomous_response))

  # Ensure the function executes without errors (indicative of successful plotting)
  expect_true(is.data.frame(plot_output))  # Confirm it still returns a data frame
})

#-------------Test point_biserial_plot------------

# Define the test cases for point_biserial_plot
test_that("point_biserial_plot returns a data frame", {
  result <- point_biserial_plot(dichotomous_response)

  # Test if the result is a data frame
  expect_true(is.data.frame(result))

  # Test if the data frame has the correct columns
  expect_named(result, c("item", "point_biserial"))
})

test_that("point_biserial_plot produces a plot", {
  # Capture plotting output
  expect_silent(plot_output <- point_biserial_plot(dichotomous_response))

  # Ensure the function executes without errors (indicative of successful plotting)
  expect_true(is.data.frame(plot_output))  # Confirm it still returns a data frame
})

#-------------Test coefficient_alpha_plot------------

data("reliability_df")

# Define the test cases for coefficient_alpha_plot
test_that("coefficient_alpha_plot returns a data frame", {
  result <- coefficient_alpha_plot(reliability_df)

  # Test if the result is a data frame
  expect_true(is.data.frame(result))

  # Test if the data frame has the correct columns
  expect_named(result, c("item", "alpha_if_dropped"))
})

test_that("coefficient_alpha_plot produces a plot", {
  # Capture plotting output
  expect_silent(plot_output <- coefficient_alpha_plot(reliability_df))

  # Ensure the function executes without errors (indicative of successful plotting)
  expect_true(is.data.frame(plot_output))  # Confirm it still returns a data frame
})
