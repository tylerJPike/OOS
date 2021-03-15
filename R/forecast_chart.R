#----------------------------------------
### Basic forecast chart
#----------------------------------------
#' Chart forecasts
#'
#' @param Data        data.frame: oos.forecast object
#' @param Title       string: chart title
#' @param Ylab        string: y-axis label
#' @param Freq        string: frequency (acts as sub-title)
#' @param zeroline    boolean: if TRUE then add a horizontal line at zero
#'
#' @return ggplot2 chart
#'
#' @examples 
#' \donttest{
#' 
#'  # simple time series
#'  A = c(1:100) + rnorm(100)
#'  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
#'  Data = data.frame(date = date, A)
#'
#'  # run forecast_univariate
#'  forecast.uni =
#'    forecast_univariate(
#'      Data = Data,
#'      forecast.dates = tail(Data$date,10),
#'      method = c('naive','auto.arima', 'ets'),
#'      horizon = 1,
#'      recursive = FALSE,
#'      freq = 'month')
#'
#'  forecasts =
#'    dplyr::left_join(
#'      forecast.uni,
#'      data.frame(date, observed = A),
#'      by = 'date'
#'    )
#'
#'  # chart forecasts
#'  chart.forecast =
#'    chart_forecast(
#'      forecasts,
#'      Title = 'test',
#'      Ylab = 'Index',
#'      Freq = 'Monthly',
#'      zeroline = TRUE)
#' 
#' }
#' 
#' @export

chart_forecast = function(
  Data,              # data.frame: oos.forecast object
  Title,             # string: chart title
  Ylab,              # string: y-axis label
  Freq,              # string: frequency (acts as sub-title)
  zeroline = FALSE   # boolean: if TRUE then add a horizontal line at zero
){

  # function errors
  if(!'forecast' %in% colnames(Data)){
    errorCondition('Data must have a column named "forecast" to calculate errors')
  }
  if(!'date' %in% colnames(Data)){
    errorCondition('Data must have a column named "date" to create plot')
  }

  # function variables
  model = observed = forecast = forecast.date = se = NA

  # reformat observed
  if('observed' %in% colnames(Data)){
    Data =
      dplyr::bind_rows(
        Data,
        Data %>% dplyr::select(forecast = observed, date) %>%
          dplyr::mutate(model = '*observed') %>%
          dplyr::distinct()
      )
  }

  # set chart
  chart =
    ggplot2::ggplot(Data, ggplot2::aes(x=date, y = forecast, color = model)) +
    # plot line
    ggplot2::geom_line(lwd = 1.25) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "grey")) +
    # chart details
    ggplot2::labs(title = Title, subtitle = Freq) +
    ggplot2::xlab("") +
    ggplot2::ylab(Ylab)

  # add zero line
  if(zeroline == TRUE){

    chart = chart +
      ggplot2::geom_hline(yintercept=0, color="black", size=.5)

  }

  return(chart)

}


#----------------------------------------
### Basic error chart
#----------------------------------------
#' Chart forecast errors
#'
#' @param Data        data.frame: oos.forecast object
#' @param Title       string: chart title
#' @param Ylab        string: y-axis label
#' @param Freq        string: frequency (acts as sub-title)
#' @param zeroline    boolean: if TRUE then add a horizontal line at zero
#'
#' @return ggplot2 chart
#' 
#' @examples 
#' \donttest{
#' 
#'  # simple time series
#'  A = c(1:100) + rnorm(100)
#'  date = seq.Date(from = as.Date('2000-01-01'), by = 'month', length.out = 100)
#'  Data = data.frame(date = date, A)
#'
#'  # run forecast_univariate
#'  forecast.uni =
#'    forecast_univariate(
#'      Data = Data,
#'      forecast.dates = tail(Data$date,10),
#'      method = c('naive','auto.arima', 'ets'),
#'      horizon = 1,
#'      recursive = FALSE,
#'      freq = 'month')
#'
#'  forecasts =
#'    dplyr::left_join(
#'      forecast.uni,
#'      data.frame(date, observed = A),
#'      by = 'date'
#'    )
#'
#'  # chart forecast errors
#'  chart.errors =
#'    chart_forecast_error(
#'      forecasts,
#'      Title = 'test',
#'      Ylab = 'Index',
#'      Freq = 'Monthly',
#'      zeroline = TRUE)
#'
#' }
#'
#' @export

chart_forecast_error = function(
  Data,              # data.frame: oos.forecast function output
  Title,             # string: chart title
  Ylab,              # string: y-axis label
  Freq,              # string: frequency (acts as sub-title)
  zeroline = FALSE   # boolean: if TRUE then add a horizontal line at zero
){

  # function errors
  if(!'observed' %in% colnames(Data)){
    errorCondition('Data must have a column named "observed" to calculate errors')
  }
  if(!'forecast' %in% colnames(Data)){
    errorCondition('Data must have a column named "forecast" to calculate errors')
  }
  if(!'date' %in% colnames(Data)){
    errorCondition('Data must have a column named "date" to create plot')
  }

  # function variables
  model = observed = forecast = forecast.date = se = errors = NA

  # calculate errors
  Data = Data %>%
    dplyr::mutate(errors = forecast - observed) %>%
    dplyr::select(date, errors, model)

  # set chart
  chart =
    ggplot2::ggplot(Data, ggplot2::aes(x=date, y = errors, color = model)) +
    # plot line
    ggplot2::geom_line(lwd = 1.25) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "grey")) +
    # chart details
    ggplot2::labs(title = Title, subtitle = Freq) +
    ggplot2::xlab("") +
    ggplot2::ylab(Ylab)

  # add zero line
  if(zeroline == TRUE){

    chart = chart +
      ggplot2::geom_hline(yintercept=0, color="black", size=.5)

  }

  return(chart)

}

