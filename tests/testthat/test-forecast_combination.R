test_that("forecast_combine produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  B = c(1:100) + rnorm(100)
  C = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A, B, C)

  # run forecast_univariate
  forecast.multi =
      forecast_multivariate(
        Data = Data,
        target = 'A',
        forecast.dates = tail(Data$date,5),
        method = c('ols','var'),
        horizon = 1,
        freq = 'month')

  forecasts =
    dplyr::left_join(
      forecast.multi,
      data.frame(date, observed = A),
      by = 'date'
    )

  # combine forecasts
  combinations =
    forecast_combine(
      forecasts,
      method = c('uniform','median','trimmed.mean',
                 'n.best','lasso'),
      burn.in = 5,
      n.max = 2)

  # expect formats
  expect_true(is.data.frame(combinations), 'forecast_combine is not a proper data.frame')

})
