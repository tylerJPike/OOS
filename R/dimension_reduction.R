
#---------------------------------------------
# Dimension reduction - Factors
#---------------------------------------------
#' Dimension reduction via factors
#'
#' A function to estimate out-of-sample principal components or partial least square scores.
#'
#' @param Data                  data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
#' @param forecast.dates        date: dates forecasts are created
#' @param target                string: column name in `Data` of variable to forecast
#' @param method               string or vector: methods to use; 'pls', ''pc
#' @param ncomp                int: number of factors to create
#' @param standardize     boolean: normalize variables (mean zero, variance one) before estimating factors
#' @param rolling.window   int: size of rolling window, NA if expanding window is used
#' @param freq               string: time series frequency; day, week, month, quarter, year; only needed for rolling window factors
#' @param horizon             int: horizon target will be forecasted (for pls)
#'
#' @return  data.frame with a date column and one column per forecast method selected
#'
#' @export

dimension_reduction = function(
  Data,                 # data.frame: data frame of target variable, exogenous variables, and observed date (named 'date')
  forecast.dates,       # date: dates forecasts are created
  target,               # string: column name in `Data` of variable to forecast
  method,               # string or vector: methods to use; 'pls', ''pc
  ncomp,                # int: number of factors to create
  standardize = TRUE,   # boolean: normalize variables (mean zero, variance one) before estimating factors
  rolling.window = NA,  # int: size of rolling window, NA if expanding window is used
  freq = NA,             # string: time series frequency; day, week, month, quarter, year; only needed for rolling window factors
  horizon = 0           # int: horizon target will be forecasted (for pls)
){

  # Create factors
  factors =
    forecast.dates %>%
      purrr::map(
        .f = function(forecast.date){

          # subset data
          # 1. using expanding window
          if(is.na(rolling.window)){
            information.set =
              dplyr::filter(Data, date <= forecast.date) %>%
              dplyr::select(-date)

            # 2. using rolling window
          }else{
            rolling.window.start = forecast.date

            if(freq == 'day'){
              rolling.window.start = forecast.date - rolling.window
            }else if(freq == 'week'){
              lubridate::week(rolling.window.start) = lubridate::week(forecast.date) - rolling.window
            }else if(freq == 'month'){
              lubridate::month(rolling.window.start) = lubridate::month(forecast.date) - rolling.window
            }else if(freq == 'quarter'){
              lubridate::month(rolling.window.start) = lubridate::month(forecast.date) - rolling.window*3
            }else if(freq == 'year'){
              lubridate::year(rolling.window.start) = lubridate::year(forecast.date) - rolling.window
            }

            information.set =
              dplyr::filter(Data, rolling.window.start <= date & date <= forecast.date ) %>%
              dplyr::select(-date)
          }

          # set target variable
          names(information.set)[names(information.set) == target] = 'target'

          # set horizon
          information.set = dplyr::mutate(information.set, target = dplyr::lead(target, horizon))

          # remove missing
          information.set = na.omit(information.set)

          # standardize variables
          information.set = information.set %>%
            dplyr::mutate_all(function(X){return((X-mean(X, na.rm = T))/sd(X, na.rm = T))})

          # estimate principal components
          if(method == 'pc'){

            # estimate factors
            model.pc = stats::princomp(dplyr::select(information.set, -target))

            # select factors
            factors = as.matrix(dplyr::select(information.set, -target)) %*% model.pc$loadings[,1:ncomp]

            # take most recent factors
            factors = factors[nrow(factors),]
            factors = t(factors)

            colnames(factors) = paste0('pc.',c(1:ncomp))

          # estimate VAR
          }else if(method == 'pls'){

            # estimate factors
            model.pls =
              plsr::pls(X = dplyr::select(information.set, -target) %>% as.matrix(),
                        Y = dplyr::select(information.set, target)  %>% as.matrix())

            # select factors
            factors = model.pls$score[,1:ncomp]
            factors = factors[nrow(factors),]

          }

          # add date
          factors = data.frame(date = forecast.date, factors)

          # return results
          return(factors)

        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)

  # return results
  return(factors)
}
