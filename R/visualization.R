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
        output_dir <- file.path("analysis", "results", "category")
    }
    file_path <- file.path(output_dir, glue::glue("count_{category}_{data_quo}.pdf"))
    ggplot2::ggsave(file_path, plot = plot, width = width, height = height, device = cairo_pdf)
}

################################################################################
# Plot grouped heatmap
################################################################################

#' @title Grouped heatmap
#' @description Plot goruped heatmap and save to folder
#' @param category character string representing the category
#' @param data data frame
#' @param label character string representing the label
#' @param cutree_rows numeric value representing the number of clusters the rows are divied into
#' @param height numeric value representing the height of the heatmap
#' @param transform boolean value representing if the data should be transposed
#' @param cutree_cols numeric value representing the number of clusters the columns are divied into, default: 8
#' @param output_dir character string representing the directory to save the heatmap, if NULL, the heatmap is saved to `/analysis/results/heatmap/`
#' @return save grouped heatmap to folder
#' @examples
#' \dontrun{
#' heatmap_group_csf(category = "dx_icd_level1", data = csf_data, label = "CSF", cutree_rows = 4, height = 5)
#'}
#' @export
heatmap_group_csf <- function(category, data, label, cutree_rows, height, transform = FALSE, cutree_cols = 8, output_dir = NULL) {
    formula <- paste0(category, "~", ".")
    phmap_data_norm <- data |>
        dplyr::select(category, granulos:lactate) |>
        tidyr::drop_na(.data[[category]]) |>
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
        output_dir <- file.path("analysis", "results", "heatmap")
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
#' @example 
#' \dontrun{
#' lapply(unique(seu_csf_train$cluster), abundanceCategoryPlot, data = abundance_combined_soupx_csf_norm_datathin)
#' }
#' @export
abundanceCategoryPlot <- function(data, cluster) {
    # Rename the variables to match the ggplot() function.
    data_plot <- dplyr::rename(data, variable = gene) |>
        dplyr::mutate(qval = -log10(qval)) |>
        dplyr::filter(cluster == {{ cluster }})

    # Calculate the height of the plot based on the number of rows in the data.
    height <- 1.5 + nrow(data_plot) * 0.1

    # Create the barplot.
    plot <- data_plot |>
        ggplot2::ggplot(ggplot2::aes(x = qval, y = ggplot2::reorder(variable, qval), fill = tfidf)) +
        ggplot2::geom_col() +
        viridis::scale_fill_viridis() +
        ggplot2::theme_classic() +
        ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", size = 1, fill = NA)) +
        ggplot2::labs(x = bquote(~ -Log[10] ~ "qval"), y = "", fill = "TF-IDF", title = cluster)

    # Save the plot to a PDF file.
    ggplot2::ggsave(file.path("analysis", "relative", "abundance", paste0("barplot_soupx_", deparse(substitute(data)), "_cluster_", cluster, ".pdf")),
        width = 6,
        height = height,
        device = grDevices::cairo_pdf
    )
}

