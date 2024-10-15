################################################################################
# Test date_distance_fun
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
