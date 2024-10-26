################################################################################
# date_distance_fun
################################################################################
test_that("date_distance_fun works correctly", {
  # Create example dates
  date1 <- as.Date("2023-01-01")
  date2 <- as.Date("2023-01-02")
  date3 <- as.Date("2023-01-10")

  # Test 1: Distance within the max_dist
  result <- date_distance_fun(date1, date2, max_dist = 2)
  expect_true(result$include) # Should be TRUE since the difference is 1 day

  # Test 2: Distance outside the max_dist
  result <- date_distance_fun(date1, date3, max_dist = 5)
  expect_false(result$include) # Should be FALSE since the difference is 9 days
})

################################################################################
# scale_this
################################################################################
test_that("scale_this works correctly", {
  # test 1: scale a vector with a mean of 0 and a standard deviation of 1
  x <- stats::rnorm(10)
  expect_equal(scale_this(x), as.vector(scale(x)))

  # test 2: scale a vector with a mean of 5 and a standard deviation of 2
  x <- stats::rnorm(10, mean = 5, sd = 2)
  expect_equal(scale_this(x), as.vector(scale(x)))

  # test 3: scale a vector with a mean of 0 and a standard deviation of 0
  x <- rep(0, 10)
  expect_equal(scale_this(x), as.vector(scale(x)))

  # # test 4: throw an error if the input is not numeric
  x <- c("1")
  expect_error(scale_this(x), "Input must be numeric")
})

################################################################################
# lm_age
################################################################################
test_that("lm_age generates a tidy data frame", {
  # Create example data
  example_data <- data.frame(
    variable = runif(10, 0, 10),
    age = runif(10, 0, 100)
  )
  age_term <- lm_age(data = example_data, variable = "variable")

  # Test 1: The output is a data frame
  expect_s3_class(age_term, "tbl_df")
  # Test 2: The output has the correct columns
  expect_equal(names(age_term), c("term", "estimate", "std.error", "statistic", "p.value"))
  # Test 3: The output has the correct number of rows
  expect_equal(nrow(age_term), 1)
})

################################################################################
# wilcox_akp_test
################################################################################

test_that("wilcox_akp_test works correctly", {
  # Create example data
  df <- data.frame(
    value = c(10.1, 20.3, 30.5, 40.7, 50.9, 60.1, 70.3, 80.5, 90.7, 100.9),
    sex = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
  )
  df$value_char <- as.character(df$value)
  df$sex_numeric <- sample(0:1, 10, replace = TRUE)

  # Run the test
  test_result <- wilcox_akp_test(df, var = "value")

  # Test 1: The output is a data frame
  expect_s3_class(test_result, "tbl_df")
  # Test 2: The output has the correct columns
  expect_equal(names(test_result), c("statistic", "p.value", "method", "alternative", "akp_effect"))
  # Test 3: The output has the correct number of rows
  expect_equal(nrow(test_result), 1)
  # Test 4 expected error if var is not numeric
  expect_error(wilcox_akp_test(df, var = "value_char"), "var must be numeric")
  # Test 5 expected error if sex is not a character
  df$sex <- sample(0:1, 10, replace = TRUE)
  expect_error(wilcox_akp_test(df, var = "value"), "sex must be character")
})
