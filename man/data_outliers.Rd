% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{data_outliers}
\alias{data_outliers}
\title{Clean outliers}
\usage{
data_outliers(
  Data,
  variables = NULL,
  w.bounds = c(0.05, 0.95),
  trim = FALSE,
  cross_section = FALSE
)
}
\arguments{
\item{Data}{data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')}

\item{variables}{string: vector of variables to standardize, default is all but 'date' column}

\item{w.bounds}{double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)}

\item{trim}{boolean: if TRUE then replace outliers with NA instead of winsorizing bound}

\item{cross_section}{boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)}
}
\value{
data.frame with a date column and one column per forecast method selected
}
\description{
A function to clean outliers. Is used as a data preparation helper function and is called internally
by forecast_univariate, forecast_multivariate, and forecast_combine.
}
