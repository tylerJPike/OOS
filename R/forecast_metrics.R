
# dependencies:
# lmtest
# sandwich
# forecast

#-------------------------------------------
# loss functions
#-------------------------------------------
#' Calculate error via loss functions
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

loss_function = function(
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
# forecast accuracy
#-------------------------------------------
#' Forecast accuracy
#'
#' A function to calculate various error loss functions. Options include:
#' MSE, RMSE, MAE, and  MAPE. The default is MSE loss.
#'
#' @param Data  data.frame: data frame of forecasts, model names, and dates
#'
#' @return data.frame of numeric error results
#'
#' @export

forecast_accuracy = function(
  Data
){

  if(!'observed' %in% names(Data)){
    print(errorCondition('There must be a column named "obsererved" in Data.'))

  }
  if(!'date' %in% names(Data)){
    print(errorCondition('There must be a column named "date" in Data.'))
  }

  # function variables
  model = observed = forecast = forecast.date = se = NA

  # set data
  information.set =
    dplyr::full_join(
      dplyr::select(Data, -observed),
      dplyr::select(Data, date, observed),
      by  = 'date')

  # calculate loss functions
  information.set = information.set %>%
    dplyr::group_split(model) %>%
    purrr::map_df(
      .f = function(X){

        Y = X %>%
          dplyr::select(observed, forecast, model) %>%
          na.omit() %>%
          dplyr::summarize(
            model = unique(model),
            MSE = mean((observed - forecast)^2, na.rm = T),
            RMSE = sqrt(mean((observed - forecast)^2, na.rm = T)),
            MAE = mean(abs(observed - forecast), na.rm = T),
            MAPE = mean(abs((forecast - observed)/observed), na.rm = T))

        return(Y)
     }
   )

  return(information.set)
}


#-------------------------------------------
# forecast comparison
#-------------------------------------------
#' Compare forecasts
#'
#' A function to compare forecasts. Options include: simple forecast error ratios,
#' Diebold-Mariano test, and Clark and West test for nested models
#'
#' @param Data                   data.frame: data frame of forecasts, model names, and dates
#' @param baseline.forecast      string: column name of baseline (null hypothesis) forecasts
#' @param test                   string: which test to use; ER = error ratio, DM = Diebold-Mariano, CM = Clark and West
#' @param loss                   string: error loss function to use if creating forecast error ratio
#' @param horizon                int: horizon of forecasts being compared in DM and CW tests
#'
#' @return numeric test result
#'
#' @export

forecast_comparison = function(
  Data,                   # data.frame: data frame of forecasts, model names, and dates
  baseline.forecast,      # string: column name of baseline (null hypothesis) forecasts
  test = 'ER',            # string: which test to use; ER = error ratio, DM = Diebold-Mariano, CM = Clark and West
  loss = 'MSE',           # string: error loss function to use if creating forecast error ratio
  horizon = NULL          # int: horizon of forecasts being compared in DM and CW tests
){

  if(!'observed' %in% names(Data)){
    print(errorCondition('There must be a column named "observed" in Data.'))

  }
  if(!'date' %in% names(Data)){
    print(errorCondition('There must be a column named "date" in Data.'))
  }

  # function variables
  model = observed = forecast = forecast.date = se = NA

  # set data
  information.set =
    dplyr::full_join(
      dplyr::select(Data, -observed),
      Data %>%
        dplyr::filter(model == baseline.forecast) %>%
        dplyr::select(date, observed, baseline.forecast = forecast),
      by  = 'date')


  # calculate loss functions
  if(test == 'ER'){
    information.set = information.set %>%
      dplyr::group_split(model) %>%
      purrr::map_df(
        .f = function(X){

          error =
            loss_function(X$forecast, X$observed, loss) /
            loss_function(X$baseline.forecast, X$observed, loss)

          return(
            data.frame(
              model = unique(X$model),
              error.ratio = error)
          )
        }
      )

  }else if(test == 'DM'){
    information.set = information.set %>%
      dplyr::group_split(model) %>%
      purrr::map_df(
        .f = function(X){

          if(sum(na.omit(X$baseline.forecast - X$forecast)) == 0){
            return(
              data.frame(
                model = baseline.forecast,
                DM.statistic = NA)
            )
          }

          DM.statistic =
            forecast::dm.test(
              e1 = na.omit(X$baseline.forecast - X$observed),
              e2 = na.omit(X$forecast - X$observed),
              alternative = 'less')$statistic[1]

          return(
            data.frame(
              model = unique(X$model),
              DM.statistic = DM.statistic)
          )
        }
      )

  }else if(test == 'CW'){
    information.set = information.set %>%
      dplyr::group_split(model) %>%
      purrr::map_df(
        .f = function(X){

          if(sum(na.omit(X$baseline.forecast - X$forecast)) == 0){
            return(
              data.frame(
                model = baseline.forecast,
                CW.statistic = NA)
            )
          }

          fCW12 =
              (X$observed - X$baseline.forecast)^2 -
              (X$observed - X$forecast)^2 -
              (X$baseline.forecast - X$forecast)^2

          lmCW = lm(as.numeric(fCW12)~1)

          lmCW.summ = summary(lmCW)

          lmCW.NW.summ = lmCW.summ

          lmCW.NW.summ$coefficients =
              unclass(lmtest::coeftest(lmCW, vcov. = sandwich::NeweyWest(lmCW, lag = horizon)))

          CW.statistic = lmCW.NW.summ$coefficients[3]

          return(
              data.frame(
                model = unique(X$model),
                Cw.statistic = CW.statistic)
          )
        }
      )
  }

  rownames(information.set) = c(1:nrow(information.set))
  return(information.set)

}
