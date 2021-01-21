test_that("forecast_univariate (direct projection) produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A)

  # run forecast_univariate
  forecast.uni =
    try(
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
        impute.missing = TRUE,
        # return
        return.models = TRUE,
        return.data = TRUE)
    )

  # expect formats
  expect_true(is.data.frame(forecast.uni$forecasts), 'forecasts is not a proper data.frame')
  expect_true(is.list(forecast.uni$models), 'models is not a proper list')
  expect_true(is.list(forecast.uni$information.set), 'information set is not a proper list')

  # expect proper names and numbers of outputs
  expect_equal(names(forecast.uni$models),  as.character(tail(Data$date,5)))
  expect_equal(names(forecast.uni$information.set),  as.character(tail(Data$date,5)))

})

test_that("forecast_univariate (recursive) produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A)

  # run forecast_univariate
  forecast.uni =
    try(
      forecast_univariate(
        Data = Data,
        forecast.dates = tail(Data$date,5),
        method = c('naive','auto.arima', 'ets'),
        horizon = 1,
        recursive = TRUE,
        # information set
        rolling.window = NA,
        freq = 'month',
        # data prep
        outlier.clean = TRUE,
        impute.missing = TRUE,
        # return
        return.models = TRUE,
        return.data = TRUE)
    )

  # expect formats
  expect_true(is.data.frame(forecast.uni$forecasts), 'rercursive forecasts is not a proper data.frame')
  expect_true(is.list(forecast.uni$models), 'rercursive models is not a proper list')
  expect_true(is.list(forecast.uni$information.set), 'rercursive information set is not a proper list')

  # expect proper names and numbers of outputs
  expect_equal(names(forecast.uni$models),  as.character(tail(Data$date,5)))
  expect_equal(names(forecast.uni$information.set),  as.character(tail(Data$date,5)))

})
