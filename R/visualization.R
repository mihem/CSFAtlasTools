################################################################################
# Plot categories
################################################################################
# This function plots a category using ggplot2.
#
# @title Plot Category
# @description This function takes a data frame and a category column as input, 
#   and returns a ggplot2 plot of the category.
#
# @param data A data frame containing the category column.
# @param category The name of the category column in the data frame.
# @param width The width of the plot.
# @param height The height of the plot.
# @param output_dir The directory to save the plot.
#
# @return A ggplot2 plot object.
#
# @examples
# data <- data.frame(
#   category = c("A", "A", "B", "B", "C", "C"),
#   value = c(1, 2, 3, 4, 5, 6)
# )
#
# category <- "category"
# # Call the plot_category function
# plot_category(data, category, width = 5, height = 5, output_dir = tempdir())
# 
# @export

plot_category <- function(data, category, width, height, output_dir) {
    plot <- dplyr::count(data, .data[[category]]) |>
        tidyr::drop_na() |>
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data[[category]], n), y = n, fill = .data[[category]])) +
        ggplot2::geom_col() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
        ) +
        ggplot2::coord_flip()
    data_quo <- deparse(substitute(data))
    file_path <- file.path(output_dir, glue::glue("count_{category}_{data_quo}.pdf"))
    ggplot2::ggsave(file_path, plot = plot, width = width, height = height, device = cairo_pdf)
}

################################################################################
# Plot grouped heatmap
################################################################################

# @title Grouped heatmap
# 
# @description Plot goruped heatmap and save to folder
# 
# @param category character string representing the category
# @param data data frame
# @param label character string representing the label
# @param cutree_rows numeric value representing the number of clusters the rows are divied into
# @param height numeric value representing the height of the heatmap
# @param transform boolean value representing if the data should be transposed
# @param cutree_cols numeric value representing the number of clusters the columns are divied into, default: 8
# @param output_dir character string representing the directory to save the heatmap.
# 
# @return save grouped heatmap to folder
# 
# @examples
# example_data <- tibble::tibble(
#   category = rep(c("A", "B", "C"), each = 3),
#   granulos = runif(9, 0, 10),
#   lactate = runif(9, 0, 5)
# )
# category <- "category"
# label <- "test_heatmap"
# heatmap_group_csf(
#   category = category,
#   data = example_data,
#   label = label,
#   cutree_rows = 3,
#   height = 8,
#   transform = FALSE,
#   cutree_cols = 2,
#   output_dir = tempdir()
# )
# @export
heatmap_group_csf <- function(category, data, label, cutree_rows, height, transform = FALSE, cutree_cols = 8, output_dir = NULL) {
    formula <- paste0(category, "~", ".")
    phmap_data_norm <- data |>
        dplyr::select(category, granulos:lactate) |>
        tidyr::drop_na(all_of(category)) |>
        recipes::recipe(stats::as.formula(formula)) |>
        bestNormalize::step_orderNorm(recipes::all_numeric()) |>
        recipes::prep() |>
        recipes::bake(new_data = NULL) |>
        dplyr::group_by(.data[[category]]) |>
        dplyr::summarize(dplyr::across(granulos:lactate, function(x) mean(x, na.rm = TRUE))) |>
        tibble::column_to_rownames(var = category)

    phmap_colors <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(n = 8, name = "RdBu")))(100)

    if (transform == TRUE) {
        phmap_data_norm <- t(phmap_data_norm)
    }

    phmap_group <- pheatmap::pheatmap(phmap_data_norm,
        color = phmap_colors,
        scale = "none",
        main = label,
        cellwidth = 10,
        cellheight = 10,
        treeheight_row = 30,
        treeheight_col = 30,
        cutree_cols = cutree_cols,
        cutree_rows = cutree_rows,
        clustering_distance_cols = "euclidean",
        clustering_distance_rows = "euclidean",
        clustering_method = "ward.D2",
        border_color = NA
    )
    file_path <- file.path(output_dir, glue::glue("hmap_{label}_{category}.pdf"))
    grDevices::cairo_pdf(file_path, width = 12, height = height)
    print(phmap_group)
    grDevices::dev.off()
}

################################################################################
# Abundance category plot
################################################################################

#' @title the abundance of each gene in a cluster as a barplot.
#'
#' @param data dataframe with the gene names, q-values and TF-IDF values.
#' @param cluster the cluster to plot.
#' @param output_dir the directory to save the plot.
#'
#' @return a ggplot2 plot saved to output_dir
#'
#' @examples
#' data <- data.frame(
#'     gene = c("Gene1", "Gene2", "Gene3"),
#'     qval = c(0.01, 0.05, 0.001),
#'     tfidf = c(0.8, 0.6, 0.9),
#'     cluster = c("Cluster1", "Cluster1", "Cluster1")
#' )
#'
#' # Define the cluster and output directory
#' cluster <- "Cluster1"
#' output_dir <- tempdir()
#'
#' # Call the abundanceCategoryPlot function
#' abundanceCategoryPlot(data, cluster, output_dir)
#' @export
abundanceCategoryPlot <- function(data, cluster, output_dir) {
    data_plot <- dplyr::rename(data, variable = gene) |>
        dplyr::mutate(qval = -log10(qval)) |>
        dplyr::filter(cluster == {{ cluster }})

    # Calculate the height of the plot based on the number of rows in the data.
    height <- 1.5 + nrow(data_plot) * 0.1

    # Create the barplot.
    plot <- data_plot |>
        ggplot2::ggplot(ggplot2::aes(x = qval, y = stats::reorder(variable, qval), fill = tfidf)) +
        ggplot2::geom_col() +
        viridis::scale_fill_viridis() +
        ggplot2::theme_classic() +
        ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", linewidth = 1, fill = NA)) +
        ggplot2::labs(x = bquote(~ -Log[10] ~ "qval"), y = "", fill = "TF-IDF", title = cluster)

    # Save the plot to a PDF file.
    data_quo <- deparse(substitute(data))
    file_path <- file.path(output_dir, glue::glue("barplot_soupx_{data_quo}_cluster_{cluster}.pdf"))

    ggplot2::ggsave(file.path(file_path),
        width = 6,
        height = height,
        device = grDevices::cairo_pdf
    )
}

################################################################################
# Correlation plots
################################################################################

#' @title Individual correlation plots of variables
#'
#' @description Create individual correlation plots of selected variables
#'
#' @param var character string representing the variable
#' @param output_dir character string representing the directory to save the
#'   plot.
#' @param estimate_df data frame containing the estimates
#' @param plot_df data frame containing the plot
#'
#' @return a ggplot2 plot saved to output_dir
#'
#' @examples
#' var <- "example_var"
#' estimate_df <- data.frame(
#'     var = c("example_var", "example_var", "example_var"),
#'     age = c(20, 30, 40),
#'     estimate = c(0.5, 0.6, 0.7),
#'     p_adjust = c(0.01, 0.02, 0.03)
#' )
#' plot_df <- data.frame(
#'     age = c(20, 30, 40),
#'     example_var = c(1, 2, 3)
#' )
#' output_dir <- tempdir()
#' corrPlot(var, estimate_df, plot_df, output_dir)
#' @export
# individul correlation plots of top variables ----
corrPlot <- function(var, estimate_df, plot_df, output_dir) {
    # Filter the data based on the variable
    result <- dplyr::filter(estimate_df, var == {{ var }})
    # Create the correlation plot
    plot <-
        plot_df |>
        ggplot2::ggplot(ggplot2::aes(x = age, y = .data[[var]])) +
        ggplot2::geom_point(size = 0.1, alpha = 0.5) +
        ggplot2::geom_smooth(method = "lm", se = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::ylab("z score") +
        ggplot2::labs(
            title = var,
            subtitle = paste0("coeff: ", signif(result$estimate, 2), ", adjusted p: ", signif(result$p_adjust, 2))
        )
    # Save the plot to a PDF file
    file_path <- file.path(output_dir, glue::glue("correlation_ctrl_age_regress_{var}.pdf"))
    ggplot2::ggsave(file_path, plot, width = 4, height = 4)
}

################################################################################
# compare variables between sex
################################################################################

#' @title Compare Variables Between Sex
#'
#' @description This function creates a boxplot to compare a variable between sexes and saves the plot to a specified directory.
#'
#' @param var A character string representing the variable to be plotted.
#' @param estimate_df A data frame containing the estimates.
#' @param plot_df A data frame containing the data for plotting.
#' @param output_dir A character string representing the directory to save the plot.
#'
#' @return A ggplot2 plot saved to output_dir.
#'
#' @examples
#' var <- "example_var"
#' estimate_df <- data.frame(
#'     var = rep("example_var", 6),
#'     sex = c("M", "F", "M", "F", "M", "F"),
#'     akp_effect = c(0.5, -0.6, 0.7, -0.8, 0.9, -1.0),
#'     p_adjust = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
#' )
#' plot_df <- data.frame(
#'     sex = c("M", "F", "M", "F", "M", "F"),
#'     example_var = c(1, 9, 6, 5, 4, 3)
#' )
#' output_dir <- tempdir()
#' compSex(var, estimate_df, plot_df, output_dir)
#' @export
compSex <- function(var, estimate_df, plot_df, output_dir) {
    # Filter the estimates data frame for the given variable
    result <- dplyr::filter(estimate_df, var == {{ var }})
    
    # Create a boxplot comparing the variable between sexes
    plot <- plot_df |>
        ggplot2::ggplot(ggplot2::aes(x = sex, y = .data[[var]])) +
        ggplot2::geom_boxplot() +
        ggplot2::theme_bw() +
        ggplot2::labs(
            title = var,
            subtitle = paste0("effect: ", signif(result$akp_effect, 2), ", adjusted p: ", signif(result$p_adjust, 2))
        ) +
        ggplot2::ylab("")
    
    # Construct the file path and save the plot
    file_path <- file.path(output_dir, glue::glue("correlation_stat_sex_regress_{var}.pdf"))
    ggplot2::ggsave(file_path, plot, width = 3, height = 4)
}

################################################################################
# compare via boxplots
################################################################################

#' @title Compare via boxplots
#' 
#' @description This function creates a boxplot to compare a parameter between groups.
#' 
#' @param par A character string representing the parameter to be plotted.
#' @param df A data frame containing the data for plotting.
#' 
#' @return A ggplot2 plot.
#' 
#' @examples
#' par <- "example_par"
#' df <- data.frame(
#'     group = c("group1", "group1", "group1", "group2", "group2", "group2"),
#'     example_par = c(3, 7, 8, 5, 9, 6)
#' )
#' compBoxplot(par, df)
#' @export
compBoxplot <- function(par, df) {
    df |>
        ggplot2::ggplot(ggplot2::aes(x = group, y = .data[[par]], fill = group)) +
        ggplot2::geom_boxplot() +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        ggplot2::theme(legend.position = "none")
}

################################################################################
# stability metric CSF
################################################################################
#' @title Stability metric for CSF
#' 
#' @description This function calculates the stability of Seurat clustering
#' at different resolutions.
#' 
#' @param t The seed number for reproducibility.
#' @param df The data frame containing the data for testing.
#' @param vars_cont Continuous variables for datathin.
#' @param vars_cat Categorical variables for datathin.
#' @param normal_estimate Estimates for normally distributed parameters for datathin.
#' @param weibull_estimate Estimates for weibull distributed parameters for datathin.
#' @param ndim Number of dimensions for Seurat
#' 
#' @return A named vector of stability values.
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'     a = rnorm(100, mean = 2, sd = 1),
#'     b = rnorm(100, mean = 2, sd = 1),
#'     x = rpois(100, lambda = 2),
#'     y = rpois(100, lambda = 2),
#'     z = rpois(100, lambda = 2)
#' )
#' vars_cont <- c("a", "b")
#' vars_cat <- c("x", "y", "z")
#' normal_estimate <-
#'     matrix(c(0.5, 0.2),
#'         nrow = nrow(df),
#'         ncol = length(vars_cont)
#'     )
#' weibull_estimate <-
#'     matrix(c(0.5, 0.2, 0.7),
#'         nrow = nrow(df),
#'         ncol = length(vars_cat)
#'     )
#' ndim <- 2
#' stabilityCSF(
#'      t = 1,
#'      df = df,
#'      vars_cont = vars_cont,
#'      vars_cat = vars_cat,
#'      normal_estimate = normal_estimate,
#'      weibull_estimate = weibull_estimate,
#'      ndim = ndim
#' )
#' @export
stabilityCSF <- function(t, df, vars_cont, vars_cat, normal_estimate, weibull_estimate, ndim) {
    # Set the seed for reproducibility
    set.seed(t)
    
    # Create the data
    data_thin1 <- datathin::datathin(df[vars_cont], family = "normal", K = 2, arg = normal_estimate)
    set.seed(t)
    data_thin2 <- datathin::datathin(df[vars_cat] + 1, family = "weibull", K = 2, arg = weibull_estimate)
    data_thin <- abind::abind(data_thin1, data_thin2, along = 2)
    data_train <- data_thin[, , 1]
    data_test <- data_thin[, , 2]
    
    # Create the Seurat objects
    seu_csf_train <- Seurat::CreateSeuratObject(t(data_train))
    seu_csf_test <- Seurat::CreateSeuratObject(t(data_test))
    
    # Preprocess the data
    seu_csf_train$RNA$data <- seu_csf_train$RNA$counts
    seu_csf_test$RNA$data <- seu_csf_test$RNA$counts

    seu_csf_train <- Seurat::FindVariableFeatures(seu_csf_train)
    seu_csf_train <- Seurat::ScaleData(seu_csf_train)
    seu_csf_train <- Seurat::RunPCA(seu_csf_train)
    seu_csf_train <- Seurat::FindNeighbors(seu_csf_train, dims = 1:ndim)

    seu_csf_test <- Seurat::FindVariableFeatures(seu_csf_test)
    seu_csf_test <- Seurat::ScaleData(seu_csf_test)
    seu_csf_test <- Seurat::RunPCA(seu_csf_test)
    seu_csf_test <- Seurat::FindNeighbors(seu_csf_test, dims = 1:ndim)
    # Calculate the stability at different resolutions
    resRange <- seq(0.2, 1.2, by = 0.1)
    resNames <- paste0("RNA_snn_res.", resRange)
    for (res in resRange) {
        seu_csf_train <- Seurat::FindClusters(seu_csf_train, resolution = res)
    }
    for (res in resRange) {
        seu_csf_test <- Seurat::FindClusters(seu_csf_test, resolution = res)
    }
    stability_res <- list()
    for (k in resNames) {
        stability_res[k] <- mclust::adjustedRandIndex(
            seu_csf_train@meta.data[[k]],
            seu_csf_test@meta.data[[k]]
        )
    }
    return(unlist(stability_res))
}

################################################################################
# stability metric blood
################################################################################
#' @title Stability metric for Blood
#'
#' @description This function calculates the stability of Seurat clustering
#' at different resolutions for blood data.
#'
#' @param t The seed number for reproducibility.
#' @param df The data frame containing the data for testing.
#' @param vars_cont Continuous variables for datathin.
#' @param normal_estimate Estimates for normally distributed parameters for datathin.
#' @param ndim Number of dimensions for Seurat.
#'
#' @return A named vector of stability values.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'     a = stats::rnorm(100, mean = 2, sd = 1),
#'     b = stats::rnorm(100, mean = 2, sd = 2),
#'     c = stats::rnorm(100, mean = 2, sd = 1),
#'     d = stats::rnorm(100, mean = 2, sd = 2),
#'     e = stats::rnorm(100, mean = 2, sd = 1)
#' )
#' vars_cont <- c("a", "b", "c", "d", "e")
#' normal_estimate <-
#'     matrix(c(0.5, 0.1, 0.7, 0.2, 0.3),
#'         nrow = nrow(df),
#'         ncol = length(vars_cont)
#'     )
#' ndim <- 2
#' stabilityBlood(
#'      t = 1,
#'      df = df,
#'      vars_cont = vars_cont,
#'      normal_estimate = normal_estimate,
#'      ndim = ndim
#' )
#' @export
stabilityBlood <- function(t, df, vars_cont, normal_estimate, ndim) {
    # Set the seed for reproducibility
    set.seed(t)
    
    # Create the data
    data_thin <- datathin::datathin(df[vars_cont], family = "normal", K = 2, arg = normal_estimate)
    data_train <- data_thin[, , 1]
    data_test <- data_thin[, , 2]
    
    # Create the Seurat objects
    seu_blood_train <- Seurat::CreateSeuratObject(t(data_train))
    seu_blood_test <- Seurat::CreateSeuratObject(t(data_test))
    
    # Preprocess the data
    seu_blood_train$RNA$data <- seu_blood_train$RNA$counts
    seu_blood_test$RNA$data <- seu_blood_test$RNA$counts

    seu_blood_train <- seu_blood_train |>
        Seurat::FindVariableFeatures() |>
        Seurat::ScaleData() |>
        Seurat::RunPCA() |>
        Seurat::FindNeighbors(dims = 1:ndim)

    seu_blood_test <- seu_blood_test |>
        Seurat::FindVariableFeatures() |>
        Seurat::ScaleData() |>
        Seurat::RunPCA() |>
        Seurat::FindNeighbors(dims = 1:ndim)
    
    # Calculate the stability at different resolutions
    resRange <- seq(0.2, 1.2, by = 0.1)
    resNames <- paste0("RNA_snn_res.", resRange)
    for (res in resRange) {
        seu_blood_train <- Seurat::FindClusters(seu_blood_train, resolution = res)
    }
    for (res in resRange) {
        seu_blood_test <- Seurat::FindClusters(seu_blood_test, resolution = res)
    }
    stability_res <- list()
    for (k in resNames) {
        stability_res[k] <- mclust::adjustedRandIndex(
            seu_blood_train@meta.data[[k]],
            seu_blood_test@meta.data[[k]]
        )
    }
    return(unlist(stability_res))
}

################################################################################
# function to plot confusion matrix
################################################################################
#' @title Plot confusion matrix
#'
#' @description This function plots the confusion matrix from a tuned
#' xgboost model.
#'
#' @param last_fit The last fit from the tuning process.
#' @param name A character string of the name of the model.
#' @param metric_df A data frame containing the metric
#' @param output_dir The directory to save the plot.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(12)
#' mtcars$cluster <- as.character(mtcars$am)
#' folds <- rsample::vfold_cv(mtcars, v = 2)
#' rec <- recipes::recipe(cluster ~ ., data = mtcars)
#' mod <- parsnip::logistic_reg()
#' control <- tune::control_resamples(save_pred = TRUE)
#' metrics <- yardstick::metric_set(
#'     yardstick::accuracy,
#'     yardstick::bal_accuracy,
#'     yardstick::f_meas,
#'     yardstick::roc_auc
#' )
#' res <- tune::fit_resamples(mod, rec, folds, control = control, metrics = metrics)
#' metric_df <- tune::collect_metrics(res)
#' metric_df$.estimate <- metric_df$mean
#' plotConfMat(res, name = "model", output_dir = tempdir(), metric_df = metric_df)
#' @export
plotConfMat <- function(last_fit, name, metric_df, output_dir) {
    plot <-
        tune::collect_predictions(last_fit) |>
        yardstick::conf_mat(truth = cluster, estimate = .pred_class) |>
        ggplot2::autoplot(type = "heatmap") +
        viridis::scale_fill_viridis() +
        ggplot2::ggtitle(glue::glue("{name}
     ROC AUC {signif(metric_df$.estimate,2)[4]},
     BACC {signif(metric_df$.estimate,2)[2]}")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.3))
    file_path <- file.path(output_dir, glue::glue("{name}_xgb_conf_mat.pdf"))
    ggplot2::ggsave(file_path, plot, width = 5, height = 5)
}

################################################################################
# function to plot variable over time
################################################################################
#' @title Time Plot
#'
#' @description This function creates a time plot for a given variable in the data.
#'
#' @param data A data frame containing the data.
#' @param var A character string representing the variable to be plotted.
#' @param size A numeric value representing the size of the points in the plot.
#' @param span A numeric value representing the span for the loess smoothing.
#'
#' @return A ggplot2 object representing the time plot.
#'
#' @examples
#' data <- data.frame(
#'     measure_time = seq(1, 100, by = 1),
#'     var1 = rnorm(100),
#'     var2 = rnorm(100)
#' )
#' TimePlot(data, var = "var1", size = 1, span = 0.5)
#' @export
TimePlot <- function(data, var, size, span) {
    mean <- base::mean(data[[var]], na.rm = TRUE)
    plot <-
        data |>
        ggplot2::ggplot(ggplot2::aes(x = measure_time, y = .data[[var]])) +
        ggplot2::geom_point(alpha = 0.3, size = size) +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::geom_smooth(method = "loess", se = TRUE, span = span, fill = "#FA8A63", color = "#FE162A") +
        ggplot2::ggtitle(var) +
        ggplot2::geom_hline(yintercept = mean, linetype = "dashed", color = "blue")
    return(plot)
}

################################################################################
# function to compare variables between clusters
################################################################################
#' @title Boxplot Cluster Manual
#'
#' @description This function creates a boxplot for a given test name and cluster, and saves the plot to a specified directory.
#'
#' @param data A data frame containing the data.
#' @param test_name A character string representing the test name to be plotted.
#' @param file_name A character string representing the file name for saving the plot.
#' @param output_dir A character string representing the directory to save the plot.
#'
#' @return None. The function saves the plot to a file.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(
#'     cluster = sample(letters[1:2], 100, replace = TRUE),
#'     test1 = stats::rnorm(100),
#'     test2 = stats::rnorm(100)
#' )
#' boxplot_cluster_manual(data, test_name = "test1", file_name = "example", output_dir = tempdir())
#' @export
boxplot_cluster_manual <- function(data, test_name, file_name, output_dir) {
    formula <- paste0(test_name, "~", "cluster")
    stat <-
        data |>
        stats::wilcox.test(stats::as.formula(formula), data = _) |>
        broom::tidy() |>
        dplyr::mutate(p.symbol = as.character(stats::symnum(p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", ""))))

    stats_list <- vector("list")
    stats_list$annotation <- stat$p.symbol
    stats_list$comparisons[[1]] <- unique(data$cluster)

    plot <-
        data |>
        ggplot2::ggplot(ggplot2::aes(x = cluster, y = .data[[test_name]], fill = cluster)) +
        ggplot2::geom_boxplot() +
        ggplot2::geom_jitter(width = 0.3, height = 0, size = .7, shape = 21, ggplot2::aes(fill = cluster)) +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        ggplot2::ylab(test_name) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        if (stat$p.value < 0.05) {
            ggsignif::geom_signif(comparisons = stats_list$comparisons, annotation = stats_list$annotation, textsize = 5, step_increase = 0.05, vjust = 0.7)
        }

    file_path <- file.path(output_dir, glue::glue("patients_cluster_manual_{file_name}_{test_name}.pdf"))
    ggplot2::ggsave(
        file_path,
        plot,
        width = 2,
        height = 5
    )
}
