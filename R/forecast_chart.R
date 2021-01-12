#----------------------------------------
### Basic chart
#----------------------------------------
#' Basic forecast chart
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

  # reformat observed
  Data =
    dplyr::bind_rows(
      Data,
      Data %>% dplyr::select(forecast = observed, date) %>%
        dplyr::mutate(model = '*observed') %>%
        dplyr::distinct()
    )

  # set chart
  chart =
    ggplot2::ggplot(Data, aes(x=date, y = forecast, color = model)) +
    # plot line
    ggplot2::geom_line(lwd = 1.25) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey")) +
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



