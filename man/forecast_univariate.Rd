% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_univariate.R
\name{forecast_univariate}
\alias{forecast_univariate}
\title{Forecast with univariate models}
\usage{
forecast_univariate(
  Data,
  forecast.dates,
  methods,
  horizon,
  recursive = TRUE,
  rolling.window = NA,
  freq,
  outlier.clean = FALSE,
  outlier.variables = NULL,
  outlier.bounds = c(0.05, 0.95),
  outlier.trim = FALSE,
  outlier.cross_section = FALSE,
  impute.missing = FALSE,
  impute.method = "kalman",
  impute.variables = NULL,
  impute.verbose = FALSE,
  parallel.dates = NULL,
  return.models = FALSE,
  return.data = FALSE
)
}
\arguments{
\item{Data}{data.frame: data frame of variable to forecast and a date column; may alternatively be a \code{ts}, \code{xts}, or \code{zoo} object to forecast}

\item{forecast.dates}{date: dates forecasts are created}

\item{methods}{string: models to estimate forecasts}

\item{horizon}{int: number of periods to forecast}

\item{recursive}{boolean: use sequential one-step-ahead forecast if TRUE, use direct projections if FALSE}

\item{rolling.window}{int: size of rolling window, NA if expanding window is used}

\item{freq}{string: time series frequency; day, week, month, quarter, year}

\item{outlier.clean}{boolean: if TRUE then clean outliers}

\item{outlier.variables}{string: vector of variables to purge of outliers, default is all but 'date' column}

\item{outlier.bounds}{double: vector of winsorizing minimum and maximum bounds, c(min percentile, max percentile)}

\item{outlier.trim}{boolean: if TRUE then replace outliers with NA instead of winsorizing bound}

\item{outlier.cross_section}{boolean: if TRUE then remove outliers based on cross-section (row-wise) instead of historical data (column-wise)}

\item{impute.missing}{boolean: if TRUE then impute missing values}

\item{impute.method}{string: select which method to use from the imputeTS package; 'interpolation', 'kalman', 'locf', 'ma', 'mean', 'random', 'remove','replace', 'seadec', 'seasplit'}

\item{impute.variables}{string: vector of variables to impute missing values, default is all numeric columns}

\item{impute.verbose}{boolean: show start-up status of impute.missing.routine}

\item{parallel.dates}{int: the number of cores available for parallel estimation}

\item{return.models}{boolean: if TRUE then return list of models estimated each forecast.date}

\item{return.data}{boolean: if True then return list of information.set for each forecast.date}
}
\value{
data.frame with a row for each forecast by model and forecasted date
}
\description{
A function to estimate univariate forecasts out-of-sample. Methods available include all forecast
methods from the \code{forecast} package. See package website for most up-to-date list of available models.
}
\examples{
\donttest{
 # simple time series
 A = c(1:100) + rnorm(100)
 date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
 Data = data.frame(date = date, A)

 # estiamte univariate forecasts
 forecast.uni =
     forecast_univariate(
       Data = Data,
       forecast.dates = tail(Data$date,5),
       method = c('naive','auto.arima', 'ets'),
       horizon = 1,
       recursive = FALSE,
       # information set
       rolling.window = NA,
       freq = 'month',
       # data prep
       outlier.clean = TRUE,
       impute.missing = TRUE)
}

}
