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

################################################################################
# Test abundacyCategoryPlot
################################################################################
test_that("corrPlot creates a ggplot and saves it correctly", {
  # Create a sample data frame
  var <- "example_var"
  estimate_df <- data.frame(
    var = c("example_var", "example_var", "example_var"),
    age = c(20, 30, 40),
    estimate = c(0.5, 0.6, 0.7),
    p_adjust = c(0.01, 0.02, 0.03)
  )
  plot_df <- data.frame(
    age = c(20, 30, 40),
    example_var = c(1, 2, 3)
  )

  # Define the output directory
  output_dir <- "."

  # Call the corrPlot function
  corrPlot(var, estimate_df, plot_df, output_dir)

  # Test 1: Function creates a file in the specified directory
  file_path <- file.path(output_dir, glue::glue("correlation_ctrl_age_regress_{var}.pdf"))
  expect_true(file.exists(file_path)) # Check if the file is created

  # Test 2: Function creates a file that is not empty
  expect_gt(file.info(file_path)$size, 0)

  # Cleanup: Remove the generated file
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
})

################################################################################
# Test compSex
################################################################################
test_that("compSex works correctly", {
  # Create a sample data frame
  var <- "example_var"
  estimate_df <- data.frame(
    var = rep("example_var", 6),
    sex = c("M", "F", "M", "F", "M", "F"),
    akp_effect = c(0.5, -0.6, 0.7, -0.8, 0.9, -1.0),
    p_adjust = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
  )
  plot_df <- data.frame(
    sex = c("M", "F", "M", "F", "M", "F"),
    example_var = c(1, 9, 6, 5, 4, 3)
  )
  output_dir <- "."
  compSex(var, estimate_df, plot_df, output_dir)

  # Test 1: Function creates a file in the specified directory
  file_path <- file.path(output_dir, glue::glue("correlation_stat_sex_regress_{var}.pdf"))
  expect_true(file.exists(file_path)) # Check if the file is created

  # Test 2: Function creates a file that is not empty
  expect_gt(file.info(file_path)$size, 0)

  # Cleanup: Remove the generated file
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
})

################################################################################
# test compBoxplot
################################################################################
test_that("compBoxplot works correctly", {
  # Create a sample data frame
  par <- "example_par"
  df <- data.frame(
    group = c("group1", "group1", "group1", "group2", "group2", "group2"),
    example_par = c(3, 7, 8, 5, 9, 6)
  )

  # Call the compBoxplot function
  plot <- compBoxplot(par, df)

  # Test 1: Function creates a plot
  expect_true(ggplot2::is.ggplot(plot)) # Check if the function returns a ggplot object

  # Test 2: Function creates a plot with two groups
  expect_equal(length(ggplot2::ggplot_build(plot)$data[[1]]$x), 2) # Check if the plot has two groups
})

################################################################################
# test stabilityCSF
################################################################################
test_that("stabilityCSF returns named vector", {
  # Create example data
  df <- data.frame(
    a = rnorm(100, mean = 2, sd = 1),
    b = rnorm(100, mean = 2, sd = 1),
    x = rpois(100, lambda = 2),
    y = rpois(100, lambda = 2),
    z = rpois(100, lambda = 2)
  )
  vars_cont <- c("a", "b")
  vars_cat <- c("x", "y", "z")
  normal_estimate <-
    matrix(c(0.5, 0.2),
      nrow = nrow(df),
      ncol = length(vars_cont)
    )
  weibull_estimate <-
    matrix(c(0.5, 0.2, 0.7),
      nrow = nrow(df),
      ncol = length(vars_cat)
    )
  ndim <- 2
  suppressWarnings(
    result <-
      stabilityCSF(
        t = 1,
        df = df,
        vars_cont = vars_cont,
        vars_cat = vars_cat,
        normal_estimate = normal_estimate,
        weibull_estimate = weibull_estimate,
        ndim = ndim
      )
  )
  # Test
  expect_type(result, "double")
  expect_equal(length(result), 11)
})

################################################################################
# Test stabilityBlood
################################################################################
test_that("stabilityBlood returns named vector", {
  # Create example data
  df <- data.frame(
    a = rnorm(100, mean = 2, sd = 1),
    b = rnorm(100, mean = 2, sd = 2),
    c = rnorm(100, mean = 2, sd = 1),
    d = rnorm(100, mean = 2, sd = 2),
    e = rnorm(100, mean = 2, sd = 1)
  )
  vars_cont <- c("a", "b", "c", "d", "e")
  normal_estimate <-
    matrix(c(0.5, 0.1, 0.7, 0.2, 0.3),
      nrow = nrow(df),
      ncol = length(vars_cont)
    )
  ndim <- 2
  suppressWarnings(
    result <-
      stabilityBlood(
        t = 1,
        df = df,
        vars_cont = vars_cont,
        normal_estimate = normal_estimate,
        ndim = ndim
      )
  )
  # Test
  expect_type(result, "double")
  expect_equal(length(result), 11)
})

################################################################################
# test plotConfMat
################################################################################

test_that("plotConfMat creates a ggplot and saves it correctly", {
    # Create a sample data frame
    mtcars$cluster <- as.character(mtcars$am)
    folds <- rsample::vfold_cv(mtcars, v = 2)
    rec <- recipes::recipe(cluster ~ ., data = mtcars)
    mod <- parsnip::logistic_reg()
    control <- tune::control_resamples(save_pred = TRUE)
    metrics <- yardstick::metric_set(
        yardstick::accuracy,
        yardstick::bal_accuracy,
        yardstick::f_meas,
        yardstick::roc_auc
    )
    res <- tune::fit_resamples(mod, rec, folds, control = control, metrics = metrics)
    metric_df <- tune::collect_metrics(res)
    metric_df$.estimate <- metric_df$mean

    # Call the plotConfMat function
    plotConfMat(res, name = "model", output_dir = ".", metric_df = metric_df)

    # Test 1: Function creates a file in the specified directory
    file_path <- file.path(".", "model_xgb_conf_mat.pdf")
    expect_true(file.exists(file_path)) # Check if the file is created

    # Test 2: Function creates a file that is not empty
    expect_gt(file.info(file_path)$size, 0)

    # Cleanup: Remove the generated file
    if (file.exists(file_path)) {
        file.remove(file_path)
    }
})

################################################################################
# test TimePlot
################################################################################

test_that("TimePlot works as expected", {
  # Create a sample data frame
  data <- data.frame(
    measure_time = seq(1, 100, by = 1),
    var1 = stats::rnorm(100),
    var2 = stats::rnorm(100)
  )

  # Call the TimePlot function
  plot <- TimePlot(data, var = "var1", size = 1, span = 0.5)

  # Test 1: Check if the function returns a ggplot object
  expect_true(ggplot2::is.ggplot(plot))

  # Test 2: Check if the plot contains points
  expect_true("GeomPoint" %in% sapply(plot$layers, function(layer) class(layer$geom)[1]))

  # Test 3: Check if the plot contains a loess smooth line
  expect_true("GeomSmooth" %in% sapply(plot$layers, function(layer) class(layer$geom)[1]))
})

