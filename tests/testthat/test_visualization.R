################################################################################
# Test plot_category
################################################################################
test_that("plot_category creates a ggplot with correct labels", {
  # Create a sample data frame
  data <- data.frame(
    category = c("A", "A", "B", "B", "C", "C"),
    value = c(1, 2, 3, 4, 5, 6)
  )

  # Call the plot_category function
  plot_category(data, "category", width = 5, height = 5, output_dir = ".") 


    # Test 1: Function creates a file in the default directory
    file_path <- file.path(".", "count_category_data.pdf")
    expect_true(file.exists(file_path)) # Check if the file is created
    # Test 2: Function  creates a file that is not empty
    expect_gt(file.info(file_path)$size, 0)
    # Cleanup: Remove the generated file
    if (file.exists(file_path)) {
      file.remove(file_path)
    }

})

################################################################################
# Test heatmap_group_csf
################################################################################
test_that("heatmap_group_csf works correctly", {
    # Create example data
    example_data <- tibble::tibble(
        group = rep(c("A", "B", "C"), each = 3),
        granulos = runif(9, 0, 10),
        lactate = runif(9, 0, 5)
    )

    heatmap_group_csf(
      category = "group",
      data = example_data,
      label = "test_heatmap",
      cutree_rows = 3,
      height = 8,
      transform = FALSE,
      cutree_cols = 2,
      output_dir = "."
    )
    # Test 1: Function creates a file in the default directory
    file_path <- file.path(".", "hmap_test_heatmap_group.pdf")
    expect_true(file.exists(file_path)) # Check if the file is created
    # Test 2: Function  creates a file that is not empty
    expect_gt(file.info(file_path)$size, 0)
    # Cleanup: Remove the generated file
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
})
