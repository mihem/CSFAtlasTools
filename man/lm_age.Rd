% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{lm_age}
\alias{lm_age}
\title{Linear model for age}
\usage{
lm_age(data, variable)
}
\arguments{
\item{data}{a data frame}

\item{variable}{a character string with the name of a column in the data frame}
}
\value{
a tidy tibble with the estimate, standard error, statistic,
and p.value for the age term
}
\description{
Create a linear model with a single predictor age for a given
variable
}
