
# dependencies:
# lmtest
# sandwich
# forecast

#-------------------------------------------
# forecast accuracy
#-------------------------------------------
#' Forecast accuracy
#'
#' A function to calculate various error loss functions. Options include:
#' MSE, RMSE, MAE, and  MAPE. The default is MSE loss.
#'
#' @param forecast  numeric: vector of forecasted values
#' @param observed  numeric: vector of observed values
#' @param metric    string: loss function
#'
#' @return numeric test result
#'
#' @export


forecast_accuracy = function(
  forecast,          # numeric: vector of forecasted values
  observed,          # numeric: vector of observed values
  metric = 'MSE'     # string: loss function
){

  if(metric == 'MSE'){
    error = mean((observed - forecast)^2, na.rm = T)
  }else if(metric == 'RMSE'){
    error = sqrt(mean((observed - forecast)^2, na.rm = T))
  }else if(metric == 'MAE'){
    error = mean(abs(observed - forecast), na.rm = T)
  }else if(metric == 'MAPE'){
    error = mean(abs((forecast - observed)/observed), na.rm = T)
  }

  return(error)
}

#-------------------------------------------
# forecast comparison
#-------------------------------------------
#' Compare forecasts
#'
#' A function to compare two forecasts. Options include: simple forecast error ratios,
#' Diebold-Mariano test, and Clark and West test for nested models
#'
#' @param baseline.forecast      numeric: vector of baseline (null hypothesis) forecasts
#' @param alternative.forecast   numeric: vector of alternative forecasts
#' @param observed               numeric: vector of observed values
#' @param test                   string: which test to use; ER = error ratio, DM = Diebold-Mariano, CM = Clark and West
#' @param loss                   string: error loss function to use if creating forecast error ratio
#' @param horizon                int: horizon of forecasts being compared in DM and CW tests
#'
#' @return numeric test result
#'
#' @export

forecast_comparison = function(
  baseline.forecast,      # numeric: vector of baseline (null hypothesis) forecasts
  alternative.forecast,   # numeric: vector of alternative forecasts
  observed,               # numeric: vector of observed values
  test = 'ER',            # string: which test to use; ER = error ratio, DM = Diebold-Mariano, CM = Clark and West
  loss = 'MSE',           # string: error loss function to use if creating forecast error ratio
  horizon = NULL          # int: horizon of forecasts being compared in DM and CW tests
){

  # Diebold-Mariano forecast comparison
  if(test == 'DM'){

    DM.statistic =
      forecast::dm.test(
        e1 = na.omit(baseline.forecast - observed),
        e2 = na.omit(alternative.forecast - observed),
        alternative = 'less')$statistic[1]

    return(DM.statistic)

  # Clark and West (2006) forecast comparison test for nested models
  }else if(test == 'CW'){

    fCW12 =
      (observed - baseline.forecast)^2 -
      (observed-alternative.forecast)^2 -
      (baseline.forecast - alternative.forecast)^2

    lmCW = lm(as.numeric(fCW12)~1)

    lmCW.summ = summary(lmCW)

    lmCW.NW.summ = lmCW.summ

    lmCW.NW.summ$coefficients = unclass(lmtest::coeftest(lmCW, vcov. = sandwich::NeweyWest(lmCW,lag=(h-1))))

    CM.statistic = lmCW.NW.summ$coefficients[3]

    return(CM.statistic)

  # Forecast error ratios
  }else if(test == 'ER'){
    error =
      forecast_accuracy(alternative.forecast, observed, loss) /
      forecast_accuracy(baseline.forecast, observed, loss)

    return(error)
  }

}









