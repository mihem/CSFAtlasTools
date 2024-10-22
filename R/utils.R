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
################################################################################
# Linear model for age
################################################################################

#' @title Linear model for age
#' 
#' @description Create a linear model with a single predictor age for a given
#' variable
#' 
#' @param data a data frame
#' @param variable a character string with the name of a column in the data frame
#' 
#' @return a tidy tibble with the estimate, standard error, statistic,
#' and p.value for the age term
#' 
#' @export
lm_age <- function(data, variable) {
  # Create the formula for the linear model
  formula <- as.formula(paste0(variable, " ~ age"))
  
  # Fit the linear model
  tidy_lm <- broom::tidy(lm(formula, data = data))
  
  # Subset the age term
  age_term <- dplyr::filter(tidy_lm, term == "age")
  return(age_term)
}

################################################################################
# Statistical test for sex
################################################################################
#' @title Wilcoxon Test with AKP Effect Size (robust version of Cohen's d)
#'
#' @description Perform a Wilcoxon test and calculate the Algina, Keselman, and Penfield (AKP) effect size. This is a robust version of Cohen's d.
#'
#' @param data A data frame containing the variables for the test.
#' @param var A character string representing the variable to be tested against 'sex'.
#'
#' @return A tidy data frame containing the Wilcoxon test results and the AKP effect size.
#'
#' @examples
#' df <- data.frame(
#'   value = c(10.1, 20.3, 30.5, 40.7, 50.9, 60.1, 70.3, 80.5, 90.7, 100.9),
#'   sex = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
#' )
#' @export
my_wilcox_test <- function(data, var) {
  # Create the formula for the Wilcoxon test
  my_formula <- paste0(var, "~ sex")
  
  # Calculate the AKP effect size using the WRS2 package
  my_formula <-  paste0(var, "~ sex")
  akp_effect <- WRS2::akp.effect(stats::as.formula(my_formula), data = data)
  
  # Perform the Wilcoxon test and convert the result to a tidy data frame
  res <- broom::tidy(stats::wilcox.test(stats::as.formula(my_formula), data = data))
  
  # Add the AKP effect size to the results
  res$akp_effect <- akp_effect$AKPeffect
  
  return(res)
}
