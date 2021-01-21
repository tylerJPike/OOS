test_that("forecast chart", {

  # simple time series
  A = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A)

  # run forecast_univariate
  forecast.uni =
    forecast_univariate(
      Data = Data,
      forecast.dates = tail(Data$date,10),
      method = c('naive','auto.arima', 'ets'),
      horizon = 1,
      recursive = FALSE,
      freq = 'month')

  forecasts =
    dplyr::left_join(
      forecast.uni,
      data.frame(date, observed = A),
      by = 'date'
    )

  # chart forecasts
  chart =
    forecast_chart(
      forecasts,
      Title = 'test',
      Ylab = 'Index',
      Freq = 'Monthly')

  expect_true(exists('chart'), 'Chart is not created.')

})
