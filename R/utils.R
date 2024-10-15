################################################################################
# Count categories
################################################################################

#' @title Count categories
#' @description count categories and save as csv
#' @param data data frame 
#' @param category character string representing the category
#' @return save categories to folder `/analysis/relative/categories/`
#' @examples \dontrun{lapply(sel_categories, count_category, data = combined_complete)}
#' @export
count_category <- function(data, category) {
    data |>
        dplyr::count(.data[[category]]) |>
        dplyr::arrange(dplyr::desc(n)) |>
        readr::write_csv(file.path("analysis", "relative", "categories", glue::glue("count_{category}.csv")))
}