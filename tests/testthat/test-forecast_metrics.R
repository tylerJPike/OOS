test_that("forecast_comparison produces standard output", {

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

  # run ER (MSE)
  er.ratio.mse =
    forecast_comparison(
      forecasts,
      baseline.forecast = 'naive',
      test = 'ER',
      loss = 'MSE')

  expect_true(is.data.frame(er.ratio.mse),'Error ratio (MSE) is not a data.frame')
  expect_true(!is.na(mean(er.ratio.mse$error.ratio)) |
                 !is.nan(mean(er.ratio.mse$error.ratio)),'Error ratio (MSE) is NA or NAN')

  # run ER (RMSE)
  er.ratio.rmse =
    forecast_comparison(
      forecasts,
      baseline.forecast = 'naive',
      test = 'ER',
      loss = 'RMSE')

  expect_true(is.data.frame(er.ratio.rmse),'Error ratio (RMSE) is not a data.frame')
  expect_true(!is.na(mean(er.ratio.rmse$error.ratio)) |
                !is.nan(mean(er.ratio.rmse$error.ratio)),'Error ratio (RMSE) is NA or NAN')

  # run ER (MAPE)
  er.ratio.mape =
    forecast_comparison(
      forecasts,
      baseline.forecast = 'naive',
      test = 'ER',
      loss = 'MAPE')

  expect_true(is.data.frame(er.ratio.mape),'Error ratio (MAPE) is not a data.frame')
  expect_true(!is.na(mean(er.ratio.mape$error.ratio)) |
                !is.nan(mean(er.ratio.mape$error.ratio)),'Error ratio (MAPE) is NA or NAN')

  # run DM test
  dm.test =
    forecast_comparison(
      forecasts,
      baseline.forecast = 'naive',
      test = 'DM')

  expect_true(is.data.frame(dm.test),'DM test is not a data.frame')
  expect_true(!is.na(mean(dm.test$error.ratio)) |
                !is.nan(mean(dm.test$error.ratio)),'DM test is NA or NAN')

})
