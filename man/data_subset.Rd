% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{data_subset}
\alias{data_subset}
\title{Create information set}
\usage{
data_subset(Data, forecast.date, rolling.window, freq)
}
\arguments{
\item{Data}{data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')}

\item{forecast.date}{date: upper bound of information set}

\item{rolling.window}{int: size of rolling window, NA if expanding window is used}

\item{freq}{string: time series frequency; day, week, month, quarter, year; only needed for rolling window factors}
}
\value{
data.frame bounded by the given date range
}
\description{
A function to subset data recursively or with a rolling window to create a valid information set. Is used as a data preparation
helper function and is called internally by forecast_univariate, forecast_multivariate, and forecast_combine.
}
