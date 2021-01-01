#' forecast_flatten
#' A function to extract point estimates for each model at each date in a list output
#' from forecast_univariate and converts them into a matrix for forecast combination routines
#' or accuracy calculations
#'
#' @param forecasts list: list of data.frames of forecasts from forecast_univariate
#'
#' @return data.frame of forecast point estiamtes
#'
#' @export

forecast_flatten = function(
  forecasts  # data.frame: data frame of forecasts from forecast_univariate
){

  models = names(forecasts[[1]])

  forecasts = forecasts %>%
    purrr::map_df(
      .f = function(data){
        data = data %>%
          purrr::map_df(.f = function(d){dplyr::select(d, forecast, date)})
        data$model = models
        data = tidyr::pivot_wider(data, names_from = model, values_from = forecast)
      }
    )

  return(forecasts)

}
