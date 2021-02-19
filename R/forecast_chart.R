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
#' @export

forecast_chart = function(
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
#' @export

forecast_error_chart = function(
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
    # dplyr::bind_rows(
    #   Data,
    #   Data %>% dplyr::select(forecast = observed, date) %>%
    #     dplyr::mutate(model = '*observed') %>%
    #     dplyr::distinct()
    # )

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

