test_that("forecast_chart produces standard output", {

  # simple time series
  A = c(1:100) + rnorm(100)
  B = c(1:100) + rnorm(100)
  C = c(1:100) + rnorm(100)
  D = c(1:100) + rnorm(100)
  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
  Data = data.frame(date = date, A, B, C, D)

  # create OOS principal components
  Data.pca =
    dimension_reduction(
      Data = Data,
      forecast.date = tail(Data$date),
      target = 'A',
      method = 'pc',
      ncomp = 2)

  expect_true(is.data.frame(Data.pca),'PC is not a data.frame')
  expect_true(!is.na(mean(Data.pca$pc.1)) |
                !is.nan(mean(Data.pca$pc.1)),'First PC is NA or NAN')


  # # create OOS principal components
  # Data.pls =
  #   dimension_reduction(
  #     Data = Data,
  #     forecast.dates = tail(Data$date),
  #     target = 'A',
  #     method = 'pls',
  #     ncomp = 1,
  #     horizon = 1)
  #
  # expect_true(is.data.frame(Data.pls),'PLS is not a data.frame')
  # expect_true(!is.na(mean(Data.pls$pc.1)) |
  #               !is.nan(mean(Data.pls$pc.1)),'First PLS score is NA or NAN')


})
