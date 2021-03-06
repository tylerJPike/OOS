% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{data_reduction}
\alias{data_reduction}
\title{Dimension reduction via principal components}
\usage{
data_reduction(Data, variables = NULL, ncomp, standardize = TRUE)
}
\arguments{
\item{Data}{data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')}

\item{variables}{string: vector of variables to standardize, default is all but 'date' column}

\item{ncomp}{int: number of factors to create}

\item{standardize}{boolean: normalize variables (mean zero, variance one) before estimating factors}
}
\value{
data.frame with a date column and one column per forecast method selected
}
\description{
A function to estimate principal components.
}
