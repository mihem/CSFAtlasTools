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

################################################################################
# Date distance
################################################################################

#' @title Calculate the distance between two dates
#' 
#' @description Calculate the absolute difference between two dates in days
#' 
#' @param v1 first date
#' @param v2 second date
#' 
#' @param max_dist maximum distance in days between the two dates
#' 
#' @return a data frame with a single column include indicating
#' whether the distance is within the maximum distance
#' 
#' @export
date_distance_fun <- function(v1, v2, max_dist = 1) {
    # Calculate the absolute difference between the two dates in days
    dist <- abs(as.double(difftime(v1, v2, units = "days")))
    
    # Create a data frame with a single column include indicating
    # whether the distance is within the maximum distance
    ret <- data.frame(include = (dist <= max_dist))
    return(ret)
}

################################################################################
# Scale variable
################################################################################

#' @title Scale a variable
#' 
#' @description Scale a variable by subtracting the mean and dividing by the standard deviation
#' 
#' @param x a numeric vector
#' 
#' @return a numeric vector with the same length as x
#' 
#' @examples
#' scale_this(1:10)
#' @export
scale_this <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
