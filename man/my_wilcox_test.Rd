% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{my_wilcox_test}
\alias{my_wilcox_test}
\title{Wilcoxon Test with AKP Effect Size (robust version of Cohen's d)}
\usage{
my_wilcox_test(data, var)
}
\arguments{
\item{data}{A data frame containing the variables for the test.}

\item{var}{A character string representing the variable to be tested against 'sex'.}
}
\value{
A tidy data frame containing the Wilcoxon test results and the AKP effect size.
}
\description{
Perform a Wilcoxon test and calculate the Algina, Keselman, and Penfield (AKP) effect size. This is a robust version of Cohen's d.
}
\examples{
df <- data.frame(
  value = c(10.1, 20.3, 30.5, 40.7, 50.9, 60.1, 70.3, 80.5, 90.7, 100.9),
  sex = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
)
}
