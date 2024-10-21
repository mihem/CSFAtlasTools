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
    
    # # test 4: scale a vector with a mean of 1 and a standard deviation of 0
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

