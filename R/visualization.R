################################################################################
# Plot categories
################################################################################
#' This function plots a category using ggplot2.
#'
#' @title Plot Category
#' @description This function takes a data frame and a category column as input, 
#'   and returns a ggplot2 plot of the category.
#'
#' @param data A data frame containing the category column.
#' @param category The name of the category column in the data frame.
#' @param width The width of the plot.
#' @param height The height of the plot.
#' @param output_dir The directory to save the plot. If NULL, the plot saved to
#'
#' @return A ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' plot_category(data = combined_complete, category = "dx_icd_level1", width = 4, height = 2)
#' }
#' @export

plot_category <- function(data, category, width, height, output_dir = NULL) {
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
    if (is.null(output_dir)) {
        output_dir <- file.path("analysis", "relative", "category")
    }
    file_path <- file.path(output_dir, glue::glue("count_{category}_{data_quo}.pdf"))
    ggplot2::ggsave(file_path, plot = plot, width = width, height = height, device = cairo_pdf)
}

################################################################################
# Plot grouped heatmap
################################################################################

#' @title Grouped heatmap
#' 
#' @description Plot goruped heatmap and save to folder
#' 
#' @param category character string representing the category
#' @param data data frame
#' @param label character string representing the label
#' @param cutree_rows numeric value representing the number of clusters the rows are divied into
#' @param height numeric value representing the height of the heatmap
#' @param transform boolean value representing if the data should be transposed
#' @param cutree_cols numeric value representing the number of clusters the columns are divied into, default: 8
#' @param output_dir character string representing the directory to save the heatmap, if NULL, the heatmap is saved to `/analysis/relative/heatmap/`
#' 
#' @return save grouped heatmap to folder
#' 
#' @examples
#' \dontrun{
#' heatmap_group_csf(category = "dx_icd_level1", data = csf_data, label = "CSF", cutree_rows = 4, height = 5)
#'}
#' @export
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
    if (is.null(output_dir)) {
        output_dir <- file.path("analysis", "relative", "heatmap")
    }
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
#' @param output_dir the directory to save the plot. If NULL, the plot is saved to `/analysis/relative/abundance/`.
#' 
#' @return a ggplot2 plot saved to output_dir
#' 
#' @examples
#' \dontrun{
#' lapply(unique(seu_csf_train$cluster), abundanceCategoryPlot, data = abundance_combined_soupx_csf_norm_datathin)
#' }
#' @export
abundanceCategoryPlot <- function(data, cluster, output_dir = NULL) {
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
    if (is.null(output_dir)) {
        output_dir <- file.path("analysis", "relative", "abundance")
    }
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
#'   plot. If NULL, the plot is saved to `/analysis/relative/relative/`.
#' @param estimate_df data frame containing the estimates
#' @param plot_df data frame containing the plot
#' 
#' @return a ggplot2 plot saved to output_dir
#' 
#' @examples
#'  var <- "example_var"
#'  estimate_df <- data.frame(
#'    var = c("example_var", "example_var", "example_var"),
#'    age = c(20, 30, 40),
#'    estimate = c(0.5, 0.6, 0.7),
#'    p_adjust = c(0.01, 0.02, 0.03)
#'  )
#'plot_df <- data.frame(
  #'age = c(20, 30, 40),
  #'example_var = c(1, 2, 3)
#')
#'output_dir <- "."
#'corrPlot(var, estimate_df, plot_df, output_dir)
#' @export
#individul correlation plots of top variables ----
corrPlot <- function(var, estimate_df, plot_df, output_dir = NULL) {
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
    if (is.null(output_dir)) {
        output_dir <- file.path("analysis", "relative", "relative")
    }

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
#'     var = c("example_var", "example_var", "example_var", "example_var", "example_var", "example_var"),
#'     sex = c("M", "F", "M", "F", "M", "F"),
#'     akp_effect = c(0.5, -0.6, 0.7, -0.8, 0.9, -1.0),
#'     p_adjust = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
#' )
#' plot_df <- data.frame(
#'     sex = c("M", "F", "M", "F", "M", "F"),
#'     example_var = c(1, 9, 6, 5, 4, 3)
#' )
#' output_dir <- "."
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
