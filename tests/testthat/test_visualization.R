################################################################################
# Test plot_category
################################################################################
test_that("plot_category creates a ggplot with correct labels", {
  # Create a sample data frame
  data <- data.frame(
    category = c("A", "A", "B", "B", "C", "C"),
    value = c(1, 2, 3, 4, 5, 6)
  )

  category <- "category"
  # Call the plot_category function
  plot_category(data, category, width = 5, height = 5, output_dir = ".")

  # Test 1: Function creates a file in the default directory
  data_quo <- "data"
  file_path <- file.path(".", glue::glue("count_{category}_{data_quo}.pdf"))
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
    category = rep(c("A", "B", "C"), each = 3),
    granulos = runif(9, 0, 10),
    lactate = runif(9, 0, 5)
  )
  category <- "category"
  label <- "test_heatmap"
  heatmap_group_csf(
    category = category,
    data = example_data,
    label = label,
    cutree_rows = 3,
    height = 8,
    transform = FALSE,
    cutree_cols = 2,
    output_dir = "."
  )

  # Test 1: Function creates a file in the default directory
  file_path <- file.path(".", glue::glue("hmap_{label}_{category}.pdf"))
  expect_true(file.exists(file_path)) # Check if the file is created
  # Test 2: Function  creates a file that is not empty
  expect_gt(file.info(file_path)$size, 0)
  # Cleanup: Remove the generated file
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
})

################################################################################
# Test abundacyCategoryPlot
################################################################################

test_that("abundanceCategoryPlot creates a ggplot and saves it correctly", {
  # Create a sample data frame
  data <- data.frame(
    gene = c("Gene1", "Gene2", "Gene3"),
    qval = c(0.01, 0.05, 0.001),
    tfidf = c(0.8, 0.6, 0.9),
    cluster = c("Cluster1", "Cluster1", "Cluster1")
  )

  # Define the cluster and output directory
  cluster <- "Cluster1"
  output_dir <- "."

  # Call the abundanceCategoryPlot function
  abundanceCategoryPlot(data, cluster, output_dir)

  # Test 1: Function creates a file in the specified directory
  data_quo <- "data"
  file_path <- file.path(output_dir, glue::glue("barplot_soupx_{data_quo}_cluster_{cluster}.pdf"))
  expect_true(file.exists(file_path)) # Check if the file is created

  # Test 2: Function creates a file that is not empty
  expect_gt(file.info(file_path)$size, 0)

  # Cleanup: Remove the generated file
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
})
