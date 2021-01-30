test_that("forecast_multivariate produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  B = c(1:100) + rnorm(100)
  C = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A, B, C)

  # run forecast_univariate
  forecast.multi =
    try(
      forecast_multivariate(
        Data = Data,
        target = 'A',
        forecast.dates = tail(Data$date,5),
        method = c('ols','var'),
        horizon = 1,
        # information set
        rolling.window = NA,
        freq = 'month',
        # data prep
        lag.n = 4,
        outlier.clean = TRUE,
        impute.missing = TRUE,
        # return
        return.models = TRUE,
        return.data = TRUE)
    )

  # expect formats
  expect_true(is.data.frame(forecast.multi$forecasts), 'forecasts is not a proper data.frame')
  expect_true(is.list(forecast.multi$models), 'models is not a proper list')
  expect_true(is.list(forecast.multi$information.set), 'information set is not a proper list')

  # expect proper names and numbers of outputs
  expect_equal(names(forecast.multi$models),  as.character(tail(Data$date,5)))
  expect_equal(names(forecast.multi$information.set),  as.character(tail(Data$date,5)))


})

test_that("forecast_multivariate produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  B = c(1:100) + rnorm(100)
  C = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A, B, C)

  # run forecast_univariate
  forecast.multi =
    try(
      forecast_multivariate(
        Data = Data,
        target = 'A',
        forecast.dates = tail(Data$date,5),
        method = c('ols','var'),
        horizon = 1,
        # information set
        rolling.window = NA,
        freq = 'month',
        # data prep
        lag.n = 4,
        outlier.clean = TRUE,
        impute.missing = TRUE,
        reduce.data = TRUE,
        reduce.ncomp = 1,
        return.models = TRUE,
        return.data = TRUE,
      )
    )

  # expect formats
  expect_true(is.data.frame(forecast.multi$forecasts), 'forecasts is not a proper data.frame')
  expect_true(is.list(forecast.multi$models), 'models is not a proper list')
  expect_true(is.list(forecast.multi$information.set), 'information set is not a proper list')

  # expect proper names and numbers of outputs
  expect_equal(names(forecast.multi$models),  as.character(tail(Data$date,5)))
  expect_equal(names(forecast.multi$information.set),  as.character(tail(Data$date,5)))

})
